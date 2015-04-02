#lang racket

(provide section define-section
         unchecked-section define-unchecked-section)

(require term-algebra/term-syntax
         term-algebra/library/module-transforms
         (prefix-in meta: term-algebra/meta)
         (prefix-in terms: term-algebra/terms)
         (only-in term-algebra/basic-api m-module)
         (for-syntax syntax/parse))

(define (prepare-import module . transforms)
  (cond
    [(empty? transforms) (meta:module-hashcode module)]
    [else
     (meta:module-hashcode
      (meta:reduce-vterm
       (term module-transforms
             (transformed-module ,module (transforms ,@transforms)))))]))

(define (combined-var-symbols section-var-symbols rule-var-symbols)
  (if (set-empty?
       (set-intersect section-var-symbols rule-var-symbols))
      (set-union section-var-symbols rule-var-symbols)
      (error "Redefinition of variable(s)")))

(define (used-section-vars left-pattern section-vars)

  (define (collect-var-refs term)
    (cond
      [(not (terms:term? term))
       (set)]
      [(equal? (terms:term-op term) 'var-ref)
       (set (first (terms:term-args term)))]
      [else
       (foldl set-union
              (set)
              (map collect-var-refs (terms:term-args term)))]))

  (define (var-symbol v)
    (first (terms:term-args (meta:vterm-term v))))

  (let ([used-var-symbols (collect-var-refs (meta:vterm-term left-pattern))])
    (filter (λ (v) (set-member? used-var-symbols
                                (var-symbol v)))
            section-vars)))

(begin-for-syntax

  (define-syntax-class module-transform
    #:description "section transform specification"
    #:attributes (transform)
    (pattern ((~datum module-name) name:id)
             #:with transform
             #'(term module-transforms
                     (module-name (quote name))))
    (pattern ((~datum add-import) import-decl:extended-import)
             #:with transform
             #'(term module-transforms
                     (add-import ,@import-decl.imports)))
    (pattern ((~datum rename-sort) name1:id name2:id)
             #:with transform
             #'(term module-transforms
                     (rename-sort (quote name1) (quote name2))))
    (pattern ((~datum rename-op) name1:id name2:id)
             #:with transform
             #'(term module-transforms
                     (rename-op (quote name1) (quote name2)))))
  
  (define-syntax-class extended-import
    #:description "import declaration"
    #:datum-literals (use include)
    #:attributes (imports sorts subsorts ops rules)
    (pattern (use module:id
                  (~optional (~seq #:transforms mt:module-transform ...) 
                             #:defaults ([(mt.transform 1) null])) ...)
             #:with imports
             #'(list (term m-module
                           (use ,(prepare-import module mt.transform ...))))
             #:with sorts #'(list)
             #:with subsorts #'(list)
             #:with ops #'(list)
             #:with rules #'(list))
    (pattern (include module:id
                      (~optional (~seq #:transforms mt:module-transform ...) 
                                 #:defaults ([(mt.transform 1) null])) ...)
             #:with imports
             #'(list (term m-module
                           (include ,(prepare-import module mt.transform ...))))
             #:with sorts #'(list)
             #:with subsorts #'(list)
             #:with ops #'(list)
             #:with rules #'(list)))
  
  (define-syntax-class sort
    #:description "sort"
    #:attributes (imports sorts subsorts ops rules)
    (pattern ((~datum sort) s-id:id)
             #:with sorts #'(list (quote s-id))
             #:with subsorts #'empty
             #:with imports #'empty
             #:with ops #'empty
             #:with rules #'empty)
    (pattern ((~datum sorts) s-id:id ...)
             #:with sorts #'(list (quote s-id) ...)
             #:with subsorts #'empty
             #:with imports #'empty
             #:with ops #'empty
             #:with rules #'empty)
    (pattern ((~datum subsort) s-id1:id s-id2:id)
             #:with sorts #'empty
             #:with subsorts
             #'(list (term m-module
                           (subsort (quote s-id1) (quote s-id2))))
             #:with imports #'empty
             #:with ops #'empty
             #:with rules #'empty)
    (pattern ((~datum subsorts) [s-id1:id s-id2:id] ...)
             #:with sorts #'empty
             #:with subsorts
             #'(list (term m-module
                           (subsort (quote s-id1) (quote s-id2))) ...)
             #:with imports #'empty
             #:with ops #'empty
             #:with rules #'empty))
  
  (define-syntax-class operator
    #:description "operator"
    #:attributes (imports sorts subsorts ops rules)
    (pattern ((~datum op) op-name:id range-sort:id)
             #:with ops
             #'(list (term m-module
                           (op (quote op-name)
                               (fixed-arity-domain)
                               (quote range-sort))))
             #:with rules #'empty
             #:with imports #'empty
             #:with sorts #'empty
             #:with subsorts #'empty)
    (pattern ((~datum op) (op-name:id arg-sort:id (~datum ...))
              range-sort:id)
             #:with ops
             #'(list (term m-module
                           (op (quote op-name)
                               (var-arity-domain (quote arg-sort))
                               (quote range-sort))))
             #:with rules #'empty
             #:with imports #'empty
             #:with sorts #'empty
             #:with subsorts #'empty)
    (pattern ((~datum op) (op-name:id arg-sort:id ...) range-sort:id)
             #:with ops
             #'(list (term m-module
                           (op (quote op-name)
                               (fixed-arity-domain (quote arg-sort) ...)
                               (quote range-sort))))
             #:with rules #'empty
             #:with imports #'empty
             #:with sorts #'empty
             #:with subsorts #'empty))

  (define-syntax-class atom
    #:description "atomic term"
    #:attributes (value)
    (pattern s:str #:with value #'s)
    (pattern ((~literal quote) symbol:id)
             #:with value #'(quote symbol))
    (pattern x:number #:when (exact? (syntax-e #'x))
             #:with value #'x))

  (define-syntax-class (term-pattern var-symbols)
    #:description "term pattern"
    #:attributes (value)
    (pattern a:atom #:with value #'a.value)
    (pattern symbol:id
             #:with value
             #`(if (set-member? #,var-symbols (quote symbol))
                   (term m-module (var-ref (quote symbol)))
                   (term m-module (term (quote symbol) (args)))))
    (pattern (symbol:id (~var arg-terms (term-pattern var-symbols)) ...)
             #:with value
             #'(term m-module
                     (pattern (quote symbol)
                              (args ,arg-terms.value ...)))))

  (define-syntax-class variable
    #:description "variable in rule"
    #:attributes (var var-symbol)
    (pattern [var-symbol:id sort-symbol:id]
             #:with var
             #'(term m-module
                     (var (quote var-symbol) (quote sort-symbol))))
    (pattern [var-symbol:id sort-symbol:id (~datum ...)]
             #:with var
             #'(term m-module
                     (svar (quote var-symbol) (quote sort-symbol)))))

  (define-splicing-syntax-class variable-list
    #:description "variable list in a rule"
    #:attributes (value symbols)
    (pattern (~seq #:var v:variable)
             #:with value #'(list v.var)
             #:with symbols #'(set (quote v.var-symbol)))
    (pattern (~seq #:vars (v:variable ...))
             #:with value #'(list v.var ...)
             #:with symbols #'(set (quote v.var-symbol) ...)))

  (define-syntax-class rule
    #:description "rule"
    #:attributes (imports sorts subsorts ops rules mvars)
    (pattern ((~datum vars) v:variable ...)
             #:with imports #'empty
             #:with sorts #'empty
             #:with subsorts #'empty
             #:with ops #'empty
             #:with rules #'empty
             #:with mvars
             #'(if (ormap (λ (s) (set-member? section-var-symbols s))
                          (list (quote v.var-symbol) ...))
                   (error "Redefinition of variable(s)")
                   (set!-values
                    (section-vars section-var-symbols)
                    (values (append (list v.var ...)
                                    section-vars)
                            (set-union (set (quote v.var-symbol) ...)
                                       section-var-symbols)))))
    (pattern ((~datum =->)
              (~optional rule-vars:variable-list
                         #:defaults ([rule-vars.value #'empty]
                                     [rule-vars.symbols #'(set)]))
              (~var left (term-pattern #'var-symbols))
              (~optional (~seq #:if (~var cond (term-pattern #'var-symbols)))
                         #:defaults ([cond.value #'(term m-module no-condition)]))
              (~var right (term-pattern #'var-symbols)))
             #:with ops #'empty
             #:with rules
             #'(let* ([var-symbols (combined-var-symbols
                                    section-var-symbols rule-vars.symbols)]
                      [used-vars (used-section-vars left.value section-vars)])
                 (list (term m-module
                             (=-> (vars ,@used-vars ,@rule-vars.value)
                                  ,left.value ,cond.value ,right.value))))
             #:with mvars #'(void)
             #:with imports #'empty
             #:with sorts #'empty
             #:with subsorts #'empty)
    (pattern ((~datum =->)
              (~optional rule-vars:variable-list
                         #:defaults ([rule-vars.value #'empty]
                                     [rule-vars.symbols #'(set)]))
              (~var left (term-pattern #'var-symbols))
              (~seq #:cond [(~var cond (term-pattern #'var-symbols))
                            (~var right (term-pattern #'var-symbols))] ...
                            [#:else (~var else (term-pattern #'var-symbols))]))
             #:with ops #'empty
             #:with rules
             
             #'(let* ([var-symbols (combined-var-symbols
                                    section-var-symbols rule-vars.symbols)]
                      [used-vars (used-section-vars left.value section-vars)])
                 (list (term m-module
                             (=-> (vars ,@used-vars ,@rule-vars.value)
                                  ,left.value ,cond.value ,right.value))
                       ...
                       (term m-module
                             (=-> (vars ,@used-vars ,@rule-vars.value)
                                  ,left.value no-condition ,else.value))))
             #:with mvars #'(void)
             #:with imports #'empty
             #:with sorts #'empty
             #:with subsorts #'empty))

  (define-syntax-class decl
    #:description "declaration in a module"
    #:attributes (imports sorts subsorts ops rules mvars)
    (pattern import-decl:extended-import
             #:with imports #'import-decl.imports
             #:with sorts #'(list)
             #:with subsorts #'(list)
             #:with ops #'(list)
             #:with mvars #'(void)
             #:with rules #'(list))
    (pattern sort-decl:sort
             #:with imports #'(list)
             #:with sorts #'sort-decl.sorts
             #:with subsorts #'sort-decl.subsorts
             #:with ops #'(list)
             #:with mvars #'(void)
             #:with rules #'(list))
    (pattern op-decl:operator
             #:with imports #'(list)
             #:with sorts #'(list)
             #:with subsorts #'(list)
             #:with ops #'op-decl.ops
             #:with mvars #'(void)
             #:with rules #'(list))
    (pattern rule-decl:rule
             #:with imports #'(list)
             #:with sorts #'(list)
             #:with subsorts #'(list)
             #:with ops #'(list)
             #:with rules #'rule-decl.rules
             #:with mvars #'rule-decl.mvars)))

(define-syntax (unchecked-section stx)
  (syntax-parse stx
    [(_ section-name:id
        declaration:decl ...)
     #'(term m-module
             (module (quote section-name)
               (imports ,@(append declaration.imports ...))
               (sorts ,@(append declaration.sorts ...))
               (subsorts ,@(append declaration.subsorts ...))
               (ops ,@(append declaration.ops ...))
               (rules ,@(let ([section-vars empty]
                              [section-var-symbols (set)])
                          declaration.mvars ...
                          (append declaration.rules ...)))))]))

(define-syntax (define-unchecked-section stx)
  (syntax-parse stx
    [(_ section-name:id decl ...)
     #'(define section-name (unchecked-section section-name decl ...))]))

(define-syntax (section stx)
  (syntax-parse stx
    [(_ arg ...)
     #'(meta:check-module (unchecked-section arg ...))]))

(define-syntax (define-section stx)
  (syntax-parse stx
    [(_ section-name:id decl ...)
     #'(define section-name (section section-name decl ...))]))
