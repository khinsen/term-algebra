#lang racket

(provide define-module define-meta-module meta-module 
         term meta-term)

(require (prefix-in modules: term-algebra/modules)
         (prefix-in terms: term-algebra/terms)
         (prefix-in meta: term-algebra/meta)
         (for-syntax syntax/parse))

(define (mterm op args)
  (terms:make-term op args (modules:module-ops meta:m-module)))

(define (pterm op args)
  (terms:make-term op args (modules:module-ops meta:m-pattern)))

(define (tterm op args)
  (terms:make-term op args (modules:module-ops meta:m-term)))

(define no-condition (mterm 'no-condition empty))

(define (unwrap-vterm vterm)
  (modules:vterm-term vterm))

(begin-for-syntax

  (define-syntax-class atom
    #:description "atomic term"
    #:attributes (value)
    (pattern s:str #:with value #'s)
    (pattern ((~literal quote) symbol:id)
             #:with value #'(quote symbol))
    (pattern x:number #:when (exact? (syntax-e #'x))
             #:with value #'x))

  (define-syntax-class term
    #:description "term"
    #:attributes (value)
    (pattern a:atom #:with value #'a.value)
    (pattern symbol:id
             #:with value
             #`(tterm 'term (list (quote symbol)
                                  (tterm 'args (list)))))
    (pattern (symbol:id arg-terms:term ...)
             #:with value
             #'(tterm 'term (list (quote symbol)
                                  (tterm 'args (list arg-terms.value ...)))))
    (pattern [[expr:expr]]
             #:with value #'(unwrap-vterm expr)))

  (define-syntax-class (term-pattern var-symbols)
    #:description "term or pattern"
    #:attributes (value)
    (pattern a:atom #:with value #'a.value)
    (pattern symbol:id
             #:with value
             #`(if (set-member? #,var-symbols (quote symbol))
                   (pterm 'var-ref (list (quote symbol)))
                   (pterm 'term (list (quote symbol)
                                  (tterm 'args (list))))))
    (pattern (symbol:id (~var arg-terms (term-pattern var-symbols)) ...)
             #:with value
             #'(pterm 'pattern (list (quote symbol)
                                     (pterm 'args (list arg-terms.value ...))))))

  (define-syntax-class variable
    #:description "variable in rule"
    #:attributes (var var-symbol)
    (pattern [var-symbol:id sort-symbol:id]
             #:with var
             #'(mterm 'var (list (quote var-symbol) (quote sort-symbol))))
    (pattern [var-symbol:id sort-symbol:id (~datum ...)]
             #:with var
             #'(mterm 'svar (list (quote var-symbol) (quote sort-symbol)))))

  (define-splicing-syntax-class variable-list
    #:description "variable list in a rule"
    #:attributes (value symbols)
    (pattern (~seq #:var v:variable)
             #:with value #'(mterm 'vars (list v.var))
             #:with symbols #'(set (quote v.var-symbol)))
    (pattern (~seq #:vars (v:variable ...))
             #:with value #'(mterm 'vars (list v.var ...))
             #:with symbols #'(set (quote v.var-symbol) ...)))

  (define-syntax-class operator
    #:description "operator/rule"
    #:attributes (ops rules)
    #:datum-literals (op =->)

    (pattern (op op-name:id range-sort:id)
             #:with ops
             #'(list (mterm 'op
                            (list (quote op-name)
                                  (mterm 'fixed-arity-domain (list))
                                  (quote range-sort))))
             #:with rules #'(list))
    (pattern (op (op-name:id arg-sort:id ...+ (~datum ...)) range-sort:id)
             #:with ops
             #'(list (mterm 'op
                            (list (quote op-name)
                                  (mterm 'var-arity-domain
                                         (list (quote arg-sort) ...))
                                  (quote range-sort))))
             #:with rules #'(list))
    (pattern (op (op-name:id arg-sort:id ...) range-sort:id)
             #:with ops
             #'(list (mterm 'op
                            (list (quote op-name)
                                  (mterm 'fixed-arity-domain
                                         (list (quote arg-sort)
                                               ...))
                                  (quote range-sort))))
             #:with rules #'(list))

    (pattern (=-> (~optional vars:variable-list
                             #:defaults ([vars.value #'(mterm 'vars empty)]
                                         [vars.symbols #'(set)]))
                  (~var left (term-pattern #'var-symbols))
                  (~optional (~seq #:if (~var cond (term-pattern #'var-symbols)))
                             #:defaults ([cond.value #'no-condition]))

                  (~var right (term-pattern #'var-symbols)))
             #:with ops #'(list)
             #:with rules
             #'(let ([var-symbols vars.symbols])
                 (list (mterm '=->
                              (list vars.value
                                    left.value cond.value right.value))))))

  (define-syntax-class import
    #:description "import"
    #:datum-literals (use include)
    (pattern (use module:id)
             #:with value #'(mterm 'use
                                   (list (modules:module-hashcode module))))
    (pattern (include module:id)
             #:with value #'(mterm 'include
                                   (list (modules:module-hashcode module)))))

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
             #:with subsorts
             #'(list (mterm 'subsort
                            (list (quote s-id1) (quote s-id2)))))
    (pattern ((~datum subsorts) [s-id1:id s-id2:id] ...)
             #:with sorts #'(list)
             #:with subsorts
             #'(list (mterm 'subsort
                            (list (quote s-id1) (quote s-id2))) ...))))

(define-syntax (meta-module stx)
  (syntax-parse stx
    [(_ module-name:id
        import-decl:import ...
        sort-decl:sort ...
        op-decl:operator ...)
     #'(modules:make-vterm meta:m-module
        (mterm 'module
               (list (quote module-name)
                     (mterm 'imports
                            (list import-decl.value ...))
                     (mterm 'sorts
                            (append sort-decl.sorts ...))
                     (mterm 'subsorts
                            (append sort-decl.subsorts ...))
                     (mterm 'ops
                            (append op-decl.ops ...))
                     (mterm 'rules
                            (append op-decl.rules ...)))))]))

(define-syntax (define-meta-module stx)
  (syntax-parse stx
    [(_ module-name:id decl ...)
     #'(define module-name (meta-module module-name decl ...))]))

(define-syntax (define-module stx)
  (syntax-parse stx
    [(_ module-name:id decl ...)
     #'(define module-name
         (meta:meta-down meta:m-module (meta-module module-name decl ...)))]))

(define-syntax (meta-term stx)
  (syntax-parse stx
    [(_  expr:term)
     #'(modules:make-vterm meta:m-term expr.value)]))

(define-syntax (term stx)
  (syntax-parse stx
    [(_ module:expr expr:term)
     #'(meta:meta-down module (modules:make-vterm meta:m-term expr.value))]))
