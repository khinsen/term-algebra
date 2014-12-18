#lang racket

(provide define-module define-meta-module
         term meta-term
         op-from make-special-rule make-special-module
         meta)

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

;
; The meta-representation of modules as terms
;
(define op-module (terms:op 'module '(name ops meta) '()))

(define op-imports (terms:op 'imports '(module ...) '()))
(define op-use (terms:op 'use '(name) '()))
(define op-extend (terms:op 'extend '(name) '()))

(define op-sorts (terms:op 'sorts '(op ...) '()))
(define op-subsorts (terms:op 'subsorts '(op ...) '()))
(define op-subsort (terms:op 'subsort '(op ...) '()))

(define op-ops (terms:op 'ops '(op ...) '()))
(define op-op (terms:op 'op '(name ...) '()))

(define op-rules (terms:op 'rules '(rule ...) '()))
(define op-eqrule (terms:op '=-> '(left right) '()))
(define op-ceqrule (terms:op '=->? '(left condition right) '()))
(define op-vars (terms:op 'vars '(...) '()))
(define op-term (terms:op 'term '(symbol ...) '()))


(define meta
  (let ([sorts (sorts:empty-sort-graph)]
        [ops (hash 'module op-module
                      'imports op-imports
                      'use op-use
                      'extend op-extend
                      'sorts op-sorts
                      'subsorts op-subsorts
                      'subsort op-subsort
                      'ops op-ops
                      'op op-op
                      'rules op-rules
                      '=-> op-eqrule
                      '=->? op-ceqrule
                      'vars op-vars
                      'term op-term)])
    (make-special-module 'meta sorts ops (set) (set 'string 'symbol))))

(define (meta-module module-name imports sorts subsorts ops rules)
  (terms:term op-module (list module-name
                              (terms:term op-imports imports)
                              (terms:term op-sorts sorts)
                              (terms:term op-subsorts subsorts)
                              (terms:term op-ops ops)
                              (terms:term op-rules rules))))

;
; Make a "compiled" module from a meta-module
;

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

(define (op-from module op-symbol)
  (get-op (module-ops module) op-symbol))

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
(define (term-from-meta ops vars term-term)

  (define (make-op-term ops vars op args)
    (let ([arg-terms (for/list ([arg args]) (term-from-meta ops vars arg))])
      (if (equal? (length arg-terms) (length (terms:op-args op)))
          (terms:term op arg-terms)
          (error "wrong number of arguments for op " op))))
  
  (define (make-var var args)
    (if (empty? args)
        var
        (error "var has non-empty argument list: " args)))
  
  (match term-term
    [(mterm op-term (cons op-or-var args))
     #:when (symbol? op-or-var)
     (cond
      [(get-op ops op-or-var)
       => (lambda (op) (make-op-term ops vars op args))]
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
     (if (has-special? ops 'exact-number)
         x
         (error "import the exact-number module to use numbers"))]
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
    (for* ([op ops])
      (terms:set-op-rules! (hash-ref copies op)
                           (map (lambda (rule) (copy-rule rule copies))
                                (terms:op-rules op))))
    (hash-values copies)))

(define (module-from-meta module-term)

  (define (do-import-sorts import-term sorts)

    (define (import-sorts import-hash sorts)
      (let ([new-sorts (module-sorts (lookup-module-hash import-hash))])
        (sorts:merge-sort-graph sorts new-sorts)))

    (match import-term
      [(mterm op-use (list import-hash))
       (import-sorts import-hash sorts)]
      [(mterm op-extend (list import-hash))
       (import-sorts import-hash sorts)]
      [_ (error "unknown import syntax " import-term)]))

  (define (do-import-ops import-term ops)
    (match import-term
      [(mterm op-use (list import-hash))
       (let* ([new-ops (module-ops (lookup-module-hash import-hash))]
              [with-special (merge-special ops new-ops)])
         (for/fold ([ops with-special])
                   ([op (all-ops new-ops)])
           (add-op ops op #t)))]
      [(mterm op-extend (list import-hash))
       (let* ([new-ops (module-ops (lookup-module-hash import-hash))]
              [with-special (merge-special ops new-ops)]
              [with-imported (for/fold ([ops with-special])
                                       ([op (all-imported-ops new-ops)])
                               (add-op ops op #t))])
         (for/fold ([ops with-imported])
                   ([op (copy-ops (all-defined-ops new-ops))])
           (add-op ops op #f)))]
      [_ (error "unknown import syntax " import-term)]))

  (define (define-sort sort-symbol sorts)
    (if (symbol? sort-symbol)
        (sorts:add-sort sorts (terms:sort sort-symbol))
        (error "not a symbol: " sort-symbol)))

  (define (define-subsort subsort-term sorts)
    (match subsort-term
      [(mterm op-subsort (list sort1 sort2))
       (sorts:add-subsort sorts (terms:sort sort1) (terms:sort sort2))]
      [_ (error "not a subsort term: " subsort-term)]))

  (define (define-op op-term ops)

    (define (op-from-meta op-term)
      (match op-term
        [(mterm op-op (cons id-symbol arg-symbols))
         #:when (and (symbol? id-symbol)
                     (list? arg-symbols)
                     (andmap symbol? arg-symbols))
         (terms:op id-symbol arg-symbols '())]
        [_ (error "not an op term: " op-term)]))
    
    (add-op ops (op-from-meta op-term) #f))

  (define (add-rule rule-term ops)

    (define (add-var var-symbol vars)
      (if (hash-has-key? vars var-symbol)
          (error "var already defined: " var-symbol)
          (hash-set vars var-symbol (terms:var var-symbol))))
  
    (define (add-rule* ops vars left right condition)
      (let* ([vars (foldl add-var (hash) vars)]
             [left-term (term-from-meta ops vars left)]
             [unused-vars (set-subtract (list->seteq (hash-values vars))
                                        (terms:vars-in-term left-term))]
             [_ (unless (set-empty? unused-vars)
                  (error (format "vars unused in left-hand-side: ~a"
                                 (set->list unused-vars))))]
             [left-op (terms:term-op left-term)]
             [right-term (term-from-meta ops vars right)]
             [cond-term (when condition
                          (term-from-meta ops vars condition))]
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
       (add-rule* ops vars left right #f)]
      [(mterm op-ceqrule (list (mterm op-vars vars) left condition right))
       (add-rule* ops vars left right condition)]
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
            [ops (foldl define-op ops op-terms)]
            [ops (foldl add-rule ops rule-terms)])
       (sorts:check-subsort-graph sorts)
       (make-module module-name sorts ops module-term))]
    [_ (error "not a meta-module: " module-term)]))

;
; Macros
;
(begin-for-syntax

  (define-syntax-class term
    #:description "term"
    (pattern symbol:id
             #:with value #'(terms:term op-term
                                        (list (quote symbol))))
    (pattern s:str #:with value #'s)
    (pattern ((~literal quote) symbol:id) #:with value #'(quote symbol))
    (pattern (op:id args:term ...)
             #:with value #'(terms:term op-term
                                        (list (quote op) args.value ...)))
    (pattern x:number #:when (exact? (syntax-e #'x))
             #:with value #'x))

  (define-syntax-class operator
    #:description "operator/rule"
    #:attributes (ops rules)
    #:datum-literals (op =->)

    (pattern (op op-name:id)
             #:with ops #'(list (terms:term op-op (list (quote op-name))))
             #:with rules #'(list))
    (pattern (op (op-name:id arg-name:id ...))
             #:with ops #'(list (terms:term op-op
                                            (list (quote op-name)
                                                  (quote arg-name) ...)))
             #:with rules #'(list))

    (pattern (=-> left:term right:term)
             #:with ops #'(list)
             #:with rules #'(list (terms:term op-eqrule
                                              (list (terms:term op-vars '())
                                                    left.value right.value))))
    (pattern (=-> #:vars (var:id ...) left:term right:term)
             #:with ops #'(list)
             #:with rules #'(list (terms:term op-eqrule
                                              (list (terms:term op-vars
                                                                (list (quote
                                                                       var)
                                                                      ...))
                                                    left.value right.value))))
    (pattern (=-> #:var var:id left:term right:term)
             #:with ops #'(list)
             #:with rules #'(list (terms:term op-eqrule
                                              (list (terms:term op-vars
                                                                (list (quote
                                                                       var)))
                                                    left.value right.value))))
    (pattern (=-> left:term  #:if cond:term right:term)
             #:with ops #'(list)
             #:with rules #'(list (terms:term op-ceqrule
                                              (list (terms:term op-vars '())
                                                    left.value
                                                    cond.value
                                                    right.value))))
    (pattern (=-> #:vars (var:id ...) left:term  #:if cond:term right:term)
             #:with ops #'(list)
             #:with rules #'(list (terms:term op-ceqrule
                                              (list (terms:term op-vars
                                                                (list (quote
                                                                       var)
                                                                      ...))
                                                    left.value
                                                    cond.value
                                                    right.value))))
    (pattern (=-> #:var var:id left:term  #:if cond:term right:term)
             #:with ops #'(list)
             #:with rules #'(list (terms:term op-ceqrule
                                              (list (terms:term op-vars
                                                                (list (quote
                                                                       var)))
                                                    left.value
                                                    cond.value
                                                    right.value)))))
  
  (define-syntax-class import
    #:description "import"
    #:datum-literals (use extend)
    (pattern (use module:id)
             #:with value #'(terms:term
                             op-use (list (module-meta-hash module))))
    (pattern (extend module:id)
             #:with value #'(terms:term
                             op-extend (list (module-meta-hash module)))))
  
  (define-syntax-class sort
    #:description "sort"
    #:attributes (sorts subsorts)
    (pattern ((~datum sort) s-id:id)
             #:with sorts #'(list (quote s-id))
             #:with subsorts #'(list))
    (pattern ((~datum sorts) s-id:id ...)
             #:with sorts #'(list (quote s-id) ...)
             #:with subsorts #'(list))
    (pattern ((~datum subsort) s-id1:id s-id2:id)
             #:with sorts #'(list)
             #:with subsorts #'(list (terms:term op-subsort
                                                 (list (quote s-id1)
                                                       (quote s-id2)))))
    (pattern ((~datum subsorts) [s-id1:id s-id2:id] ...)
             #:with sorts #'(list)
             #:with subsorts #'(list (terms:term op-subsort
                                                 (list (quote s-id1)
                                                       (quote s-id2))) ...))))

(define-syntax (define-meta-module stx)
  (syntax-parse stx
    [(_ module-name:id
        import-decl:import ...
        sort-decl:sort ...
        op-decl:operator ...)
     #'(define module-name
         (meta-module (quote module-name)
                      (list import-decl.value ...)
                      (append sort-decl.sorts ...)
                      (append sort-decl.subsorts ...)
                      (append op-decl.ops ...)
                      (append op-decl.rules ...)))]))

(define-syntax (define-module stx)
  (syntax-parse stx
    [(_ module-name:id
        import-decl:import ...
        sort-decl:sort ...
        op-decl:operator ...)
     #'(define module-name
         (module-from-meta (meta-module (quote module-name)
                                        (list import-decl.value ...)
                                        (append sort-decl.sorts ...)
                                        (append sort-decl.subsorts ...)
                                        (append op-decl.ops ...)
                                        (append op-decl.rules ...))))]))

(define-syntax (meta-term stx)
  (syntax-parse stx
    [(_  expr:term)
     #'expr.value]))

(define-syntax (term stx)
  (syntax-parse stx
    [(_ module:expr expr:term)
     #'(term-from-meta (module-ops module) (hash) expr.value)]))
