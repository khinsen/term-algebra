#lang racket

(provide (struct-out module)
         define-builtin-module
         sort-from op-from
         make-special-rule make-special-module
         module-from-meta term-from-meta meta-module make-mterm
         metalevel-term metalevel-module)

(require (prefix-in terms: term-algebra/terms)
         (prefix-in sorts: term-algebra/sorts)
         (for-syntax syntax/parse)
         (only-in file/sha1 sha1))

;
; The data structure for the internal module representation
;
(struct ops-in-module (ops imported-ops special-ops)
        #:transparent)

(struct module (name sorts ops meta meta-hash)
        #:transparent)

;
; The module registry
;
(define (hash-of-string s)
  (sha1 (open-input-string s)))

(define (hash-of-meta-module meta-module)
  (let ([o (open-output-string)])
    (write meta-module o)
    (hash-of-string (get-output-string o))))

(define *modules* (make-hash))

(define (register-module module)
  (hash-set! *modules* (module-meta-hash module) (make-weak-box module)))

(define (lookup-module-hash hash)
  (weak-box-value (hash-ref *modules* hash)))

(define (make-module module-name sorts ops meta-terms)
  (let* ([meta-hash (if meta-terms
                        (hash-of-meta-module meta-terms)
                        (hash-of-string (symbol->string module-name)))]
         [mod (module module-name sorts ops meta-terms meta-hash)])
    (register-module mod)
    mod))

(define (make-special-module module-name sorts
                             defined-ops imported-ops special-ops)
  (let* ([meta-hash (hash-of-string (symbol->string module-name))]
         [ops (ops-in-module defined-ops imported-ops special-ops)]
         [mod (module module-name sorts ops #f meta-hash)])
    (register-module mod)
    mod))

; Manage the module-ops data structure that combines
; a hash map for the operators with a set flagging the
; imported operators.
(define (empty-ops)
  (ops-in-module (hash) (set) (set)))

(define (add-op ops op imported?)
  (let* ([all-ops (ops-in-module-ops ops)]
         [imported (ops-in-module-imported-ops ops)]
         [special (ops-in-module-special-ops ops)]
         [op-symbol (terms:op-symbol op)])
    (when (hash-has-key? all-ops op-symbol)
        (error "op already defined: " op-symbol))
    (ops-in-module (hash-set all-ops op-symbol op)
                   (if imported?
                       (set-add imported op-symbol)
                       imported)
                   special)))

(define (get-op ops symbol)
  (hash-ref (ops-in-module-ops ops) symbol #f))

(define (all-ops ops)
  (hash-values (ops-in-module-ops ops)))

(define (all-imported-ops ops)
  (filter (lambda (op) (set-member? (ops-in-module-imported-ops ops) 
                               (terms:op-symbol op)))
          (hash-values (ops-in-module-ops ops))))

(define (all-defined-ops ops)
  (filter (lambda (op) (not (set-member? (ops-in-module-imported-ops ops) 
                                    (terms:op-symbol op))))
          (hash-values (ops-in-module-ops ops))))

(define (op-is-imported? ops symbol)
  (and (hash-has-key? (ops-in-module-ops ops) symbol)
       (set-member? (ops-in-module-imported-ops ops) symbol)))

(define (sort-from module sort-symbol)
  (let ([sort sort-symbol])
    (if (sorts:has-sort? sort (module-sorts module))
        sort
        (error (format "no sort ~s in module ~s" sort-symbol module)))))

(define (op-from module op-symbol)
  (get-op (module-ops module) op-symbol))

(define (import-ops new-ops mark-imported? ops)
  (let ([with-special (merge-special ops new-ops)])
    (if mark-imported?
        (for/fold ([ops with-special])
                  ([op (all-ops new-ops)])
          (add-op ops op mark-imported?))
        (let ([with-imported (for/fold ([ops with-special])
                                       ([op (all-imported-ops new-ops)])
                               (add-op ops op #t))])
          (for/fold ([ops with-imported])
                    ([op (copy-ops (all-defined-ops new-ops))])
            (add-op ops op mark-imported?))))))

(define (make-special-rule module op-symbol proc)
  (let ([op (op-from module op-symbol)])
    (if (empty? (terms:op-rules op))
        (terms:set-op-rules! op proc)
        (error "non-empty rule list for operator " op))))

(define (merge-special ops added-ops)
  (let* ([defined-ops (ops-in-module-ops ops)]
         [imported (ops-in-module-imported-ops ops)]
         [special (ops-in-module-special-ops ops)]
         [added-special (ops-in-module-special-ops added-ops)])
    (ops-in-module defined-ops
                   imported
                   (set-union special added-special))))

(define (has-special? ops symbol)
  (set-member? (ops-in-module-special-ops ops) symbol))

; Make a valid term in the context of a module

(define (make-term op args sorts ops vars)

  (define (make-fix-arg-op-term op args sorts ops vars)
    (when (not (equal? (length args) (length (terms:op-domain op))))
      (error "wrong number of arguments for op " op))
    (for ([arg args]
          [sort (terms:op-domain op)])
      (let ([tsort (terms:term-sort arg)])
        (when (not (sorts:is-sort? tsort sort sorts))
          (error (format "sort ~s not compatible with ~s" tsort sort)))))
    (terms:term op args))

  (define (make-var-arg-op-term op args sorts ops vars)
    (let* ([n-fix-args (- (length (terms:op-domain op)) 1)]
           [n-var-args (- (length args) n-fix-args)])
      (when (negative? n-var-args)
        (error "too few arguments for op " op))
      (let-values ([(fix-sorts var-sort)
                    (split-at-right (terms:op-domain op) 1)])
        (for ([arg args]
              [sort (append fix-sorts
                            (for/list ([n (range n-var-args)])
                              (first var-sort)))])
          (let ([tsort (terms:term-sort arg)])
            (when (not (sorts:is-sort? tsort sort sorts))
              (error (format "sort ~s not compatible with ~s" tsort sort))))))
      (terms:term op args)))

  (if (set-member? (terms:op-properties op)
                   'variable-length-domain)
      (make-var-arg-op-term op args sorts ops vars)
      (make-fix-arg-op-term op args sorts ops vars)))

;
; Macro for defining builtin modules
;
(define-syntax (define-builtin-module stx)

  (define-syntax-class import-builtin
    #:description "import for builtin modules"
    #:attributes (module use)
    (pattern ((~datum use) module:id)
             #:with use #'#t)
    (pattern ((~datum extend) module:id)
             #:with use #'#f))
  
  (define-syntax-class op-builtin
    #:description "operator/rule for builtin modules"
    #:attributes (ops)

    (pattern ((~datum op) op-name:id range-sort:id
              (~optional rules:expr #:defaults ([rules #'(list)])))
             #:with ops 
             #'(list (terms:op (quote op-name)
                               '()
                               (quote range-sort)
                               (set)
                               rules)))
    (pattern ((~datum op)
              (op-name:id arg-sort:id ...+ (~datum ...))
              range-sort:id
              (~optional rules:expr #:defaults ([rules #'(list)])))
             #:with ops
             #'(list (terms:op (quote op-name)
                               (list (quote arg-sort) ...)
                               (quote range-sort)
                               (set 'variable-length-domain)
                               rules)))
    (pattern ((~datum op) (op-name:id arg-sort:id ...+) range-sort:id
              (~optional rules:expr #:defaults ([rules #'(list)])))
             #:with ops
             #'(list (terms:op (quote op-name)
                               (list (quote arg-sort) ...)
                               (quote range-sort)
                               (set)
                               rules))))

  (syntax-parse stx
    [(_ module-name:id
        import-decl:import-builtin ...
        (~optional
         ((~datum sorts) sort:id ...))
        (~optional
         ((~datum subsorts) (sort1:id sort2:id) ...))
        (~optional
         ((~datum special-ops) s-op:id ...))
        op-decl:op-builtin ...)
     (with-syntax ([sort-import-list (if (attribute import-decl.module)
                                         #'(list (module-sorts import-decl.module) ...)
                                         #'(list))]
                   [op-import-list (if (attribute import-decl.module)
                                       #'(map (lambda (a b) (cons a b))
                                          (list (module-ops import-decl.module) ...)
                                          (list import-decl.use ...))
                                       #'(list))]
                   [sort-list (if (attribute sort)
                                  #'(list (quote sort) ...)
                                  #'(list))]
                   [subsort-list (if (attribute sort1)
                                     #'(list (cons (quote sort1)
                                                   (quote sort2))
                                             ...)
                                     #'(list))]
                   [special-op-set (if (attribute s-op)
                                        #'(set (quote s-op) ...)
                                        #'(set))]
                   [op-list (if (attribute op-decl.ops)
                                #'(append op-decl.ops ...)
                                #'(list))])
       #'(define module-name
           (let* ([sorts (sorts:empty-sort-graph)]
                  [sorts (foldl sorts:merge-sort-graph sorts sort-import-list)]
                  [sorts (foldl sorts:add-sort sorts sort-list)]
                  [sorts (for/fold ([sorts sorts])
                                   ([sort-pair subsort-list])
                           (sorts:add-subsort (car sort-pair) (cdr sort-pair)
                                              sorts))]
                  [ops (ops-in-module (hash) (set) special-op-set)]
                  [ops (for/fold ([ops ops]) 
                                 ([mod op-import-list])
                         (import-ops (car mod) (cdr mod) ops))]
                  [ops (for/fold ([ops ops]) 
                                 ([op op-list])
                         (add-op ops op #f))]
                  [meta-hash (hash-of-string
                              (symbol->string (quote module-name)))]
                  [mod (module (quote module-name) sorts ops #f meta-hash)])
             (register-module mod)
             mod)))]))

;
; The meta-representation of terms and modules as terms
;
(define-builtin-module metalevel-term

  ; can't write
  ;   (use builtin:rational)
  ;   (use builtin:string)
  ;   (use builtin:symbol)
  ; here because builtin.rkt depends on modules.rkt.

  (sorts Rational String Symbol
         Term)

  (subsorts [Rational Term] [String Term] [Symbol Term])

  (special-ops rational-number string symbol)

  (op (term Symbol Term ...) Term))

(define-builtin-module metalevel-module

  (use metalevel-term)

  (sorts Module
         ImportList Import
         SortList SubsortList Subsort
         OpList Op Domain
         RuleList Rule VarList Var)

  (op (module Symbol ImportList SortList SubsortList OpList RuleList) Module)
  
  (op (imports Import ...) ImportList)
  (op (use String) Import)
  (op (extend String) Import)
  
  (op (sorts Symbol ...) SortList)
  
  (op (subsorts Subsort ...) SubsortList)
  (op (subsort Symbol Symbol) Subsort)
  
  (op (ops Op ...) OpList)
  (op (op Symbol Domain Symbol) Op)
  (op (domain Symbol ...) Domain)
  (op (var-length-domain Symbol ...) Domain)
  
  (op (rules Rule ...) RuleList)
  (op (=-> VarList Term Term) Rule)
  (op (=->? VarList Term Term Term) Rule)
  (op (vars Var ...) VarList)
  (op (var Symbol Symbol) Var))


(define op-module (op-from metalevel-module 'module))

(define op-imports (op-from metalevel-module 'imports))
(define op-use (op-from metalevel-module 'use))
(define op-extend (op-from metalevel-module 'extend))
  
(define op-sorts (op-from metalevel-module 'sorts))
  
(define op-subsorts (op-from metalevel-module 'subsorts))
(define op-subsort (op-from metalevel-module 'subsort))
  
(define op-ops (op-from metalevel-module 'ops))
(define op-op (op-from metalevel-module 'op))
(define op-domain (op-from metalevel-module 'domain))
(define op-var-length-domain (op-from metalevel-module 'var-length-domain))
  
(define op-rules (op-from metalevel-module 'rules))
(define op-eqrule (op-from metalevel-module '=->))
(define op-ceqrule (op-from metalevel-module '=->?))
(define op-vars (op-from metalevel-module 'vars))
(define op-var (op-from metalevel-module 'var))
(define op-term (op-from metalevel-module 'term))

(define no-vars (hash))

(define (make-mterm op args)
  (make-term op args
             (module-sorts metalevel-module)
             (module-ops metalevel-module)
             no-vars))

(define (meta-module module-name imports sorts subsorts ops rules)
  (make-mterm op-module
              (list module-name
                    (make-mterm op-imports imports)
                    (make-mterm op-sorts sorts)
                    (make-mterm op-subsorts subsorts)
                    (make-mterm op-ops ops)
                    (make-mterm op-rules rules))))

;
; Make a "compiled" module from a meta-module
;

; Two match expanders for simplifying the parser for meta-modules.
(define-match-expander mterm
  (lambda (stx)
    (syntax-parse stx
      [(_ an-op some-args)
       #'(struct* terms:term ([op (==  an-op eq?)] [args some-args]))])))

(define-match-expander mterm0
  (lambda (stx)
    (syntax-parse stx
      [(_ an-op)
       #'(struct* terms:term ([op (==  an-op eq?)] [args (list)]))])))

; Convert a meta-term to a concrete term
(define (term-from-meta sorts ops vars term-term)

  (define (make-op-term sorts ops vars op args)
    (let ([arg-terms (for/list ([arg args])
                       (term-from-meta sorts ops vars arg))])
      (make-term op arg-terms sorts ops vars)))
  
  (define (make-var var args)
    (if (empty? args)
        var
        (error "var has non-empty argument list: " args)))
  
  (match term-term
    [(mterm op-term (cons op-or-var args))
     #:when (symbol? op-or-var)
     (cond
      [(get-op ops op-or-var)
       => (lambda (op) (make-op-term sorts ops vars op args))]
      [(hash-has-key? vars op-or-var)
       (make-var (hash-ref vars op-or-var) args)]
      [else (error "undefined op or var: " op-or-var)])]
    [s #:when (string? s)
     (if (has-special? ops 'string)
         s
         (error "import the string module to use strings"))]
    [s #:when (symbol? s)
     (if (has-special? ops 'symbol)
         s
         (error "import the symbol module to use symbols"))]
    [x #:when (exact? x)
     (if (has-special? ops 'rational-number)
         x
         (error "import the rational module to use numbers"))]
    [_ (error "not a meta-term: " term-term)]))

; Convert a meta-module to a concrete module

(define (copy-ops ops)

  (define (copy-term term ops-map)
    (if (terms:term? term)
        (let ([op (terms:term-op term)]
              [args (terms:term-args term)])
          (struct-copy terms:term term
                       [op (hash-ref ops-map op op)]
                       [args (map (lambda (a) (copy-term a ops-map)) args)]))
        term))

  (define (copy-rule rule ops-map)
    (if (procedure? rule)
        rule
        (cons (copy-term (car rule) ops-map)
              (let ([rest (cdr rule)])
                (if (pair? rest)
                    (copy-rule rest ops-map)
                    (copy-term rest ops-map))))))

  (let* ([copies (make-hasheq
                  (map (lambda (op)
                         (cons op (struct-copy terms:op op [rules '()])))
                       ops))])
    (for ([op ops])
      (terms:set-op-rules! (hash-ref copies op)
                           (map (lambda (rule) (copy-rule rule copies))
                                (terms:op-rules op))))
    (hash-values copies)))

(define (module-from-meta module-term)

  (define (do-import-sorts import-term sorts)

    (define (import-sorts import-hash sorts)
      (let ([new-sorts (module-sorts (lookup-module-hash import-hash))])
        (sorts:merge-sort-graph new-sorts sorts)))

    (match import-term
      [(mterm op-use (list import-hash))
       (import-sorts import-hash sorts)]
      [(mterm op-extend (list import-hash))
       (import-sorts import-hash sorts)]
      [_ (error "unknown import syntax " import-term)]))

  (define (do-import-ops import-term ops)
    (match import-term
      [(mterm op-use (list import-hash))
       (import-ops (module-ops (lookup-module-hash import-hash)) #t ops)]
      [(mterm op-extend (list import-hash))
       (import-ops (module-ops (lookup-module-hash import-hash)) #f ops)]
      [_ (error "unknown import syntax " import-term)]))

  (define (define-sort sort-symbol sorts)
    (if (symbol? sort-symbol)
        (sorts:add-sort sort-symbol sorts)
        (error "not a symbol: " sort-symbol)))

  (define (define-subsort subsort-term sorts)
    (match subsort-term
      [(mterm op-subsort (list sort1 sort2))
       (sorts:add-subsort sort1 sort2 sorts)]
      [_ (error "not a subsort term: " subsort-term)]))

  (define (define-op sorts ops op-term)

    (define (op-from-meta op-term)

      (define (op-from-meta* name arg-sorts range-sort properties)
        (let ([range-sort range-sort]
              [arg-sorts arg-sorts])
          (unless (sorts:has-sort? range-sort sorts)
            (error "undefined sort: " range-sort))
          (for ([arg-sort arg-sorts])
            (unless (sorts:has-sort? arg-sort sorts)
              (error "undefined sort: " arg-sort)))
          (terms:op name arg-sorts range-sort properties '())))

      (match op-term
        [(mterm op-op (list name (mterm op-domain arg-sorts) range-sort))
         #:when (and (symbol? name)
                     (list? arg-sorts)
                     (andmap symbol? arg-sorts)
                     (symbol? range-sort))
         (op-from-meta* name arg-sorts range-sort (set))]
        [(mterm op-op (list name (mterm op-var-length-domain arg-sorts)
                            range-sort))
         #:when (and (symbol? name)
                     (list? arg-sorts)
                     (andmap symbol? arg-sorts)
                     (symbol? range-sort))
         (op-from-meta* name arg-sorts range-sort
                        (set 'variable-length-domain))]
        [_ (error "not an op term: " op-term)]))
    
    (add-op ops (op-from-meta op-term) #f))

  (define (add-rule sorts ops rule-term)

    (define (add-var var-term vars)
      (match var-term
        [(mterm op-var (list name-symbol sort-symbol))
         (if (hash-has-key? vars name-symbol)
             (error "var already defined: " name-symbol)
             (hash-set vars name-symbol (terms:var name-symbol
                                                   sort-symbol)))]
        [_ (error "not a var terms: " var-term)]))
  
    (define (add-rule* sorts ops vars left right condition)
      (let* ([vars (foldl add-var (hash) vars)]
             [left-term (term-from-meta sorts ops vars left)]
             [unused-vars (set-subtract (list->seteq (hash-values vars))
                                        (terms:vars-in-term left-term))]
             [_ (unless (set-empty? unused-vars)
                  (error (format "vars unused in left-hand-side: ~a"
                                 (set->list unused-vars))))]
             [left-op (terms:term-op left-term)]
             [right-term (term-from-meta sorts ops vars right)]
             [cond-term (when condition
                          (term-from-meta sorts ops vars condition))]
             [rule (if condition
                       (list (cons left-term
                                   (cons cond-term
                                         right-term)))
                       (list (cons left-term
                                   right-term)))])
        (if (op-is-imported? ops (terms:op-symbol left-op))
            (error "cannot add rule to imported operator" left-op)
            (terms:set-op-rules! left-op
                                 (append (terms:op-rules left-op)
                                         rule)))))

    (match rule-term
      [(mterm op-eqrule (list (mterm op-vars vars) left right))
       (add-rule* sorts ops vars left right #f)]
      [(mterm op-ceqrule (list (mterm op-vars vars) left condition right))
       (add-rule* sorts ops vars left right condition)]
      [_ (error "not a rule: " rule-term)])

    ops)

  (match module-term
    [(mterm op-module (list module-name
                            (mterm op-imports import-terms)
                            (mterm op-sorts sort-terms)
                            (mterm op-subsorts subsort-terms)
                            (mterm op-ops op-terms)
                            (mterm op-rules rule-terms)))
     #:when (symbol? module-name)
     (let* ([sorts (sorts:empty-sort-graph)]
            [ops (empty-ops)]
            [sorts (foldl do-import-sorts sorts import-terms)]
            [ops (foldl do-import-ops ops import-terms)]
            [sorts (foldl define-sort sorts sort-terms)]
            [sorts (foldl define-subsort sorts subsort-terms)]
            [ops (for/fold ([ops ops])
                           ([op-term op-terms])
                   (define-op sorts ops op-term))]
            [ops (for/fold ([ops ops])
                           ([rule-term rule-terms])
                   (add-rule sorts ops rule-term))])
       (sorts:check-subsort-graph sorts)
       (make-module module-name sorts ops module-term))]
    [_ (error "not a meta-module: " module-term)]))
