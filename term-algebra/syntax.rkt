#lang racket

(provide define-module define-meta-module
         term meta-term)

(require (prefix-in modules: term-algebra/modules)
         (prefix-in terms: term-algebra/terms)
         (prefix-in meta: term-algebra/meta)
         (for-syntax syntax/parse))

(define meta-term-ops (modules:module-ops meta:meta-term))
(define meta-module-ops (modules:module-ops meta:meta-module))

(define (mterm op args)
  (terms:make-term op args meta-module-ops))

(define (tterm op args)
  (terms:make-term op args meta-term-ops))

(begin-for-syntax

  (define-syntax-class term
    #:description "term"
    (pattern symbol:id
             #:with value
             #'(tterm 'term (list (quote symbol)
                                  (mterm 'args (list)))))
    (pattern s:str #:with value #'s)
    (pattern ((~literal quote) symbol:id)
             #:with value #'(quote symbol))
    (pattern (symbol:id arg-terms:term ...)
             #:with value
             #'(tterm 'term (list (quote symbol)
                                  (mterm 'args (list arg-terms.value ...)))))
    (pattern x:number #:when (exact? (syntax-e #'x))
             #:with value #'x))

  (define-syntax-class variable
    #:description "variable in rule"
    #:attributes (var)
    (pattern [var-name:id var-sort:id]
             #:with var
             #'(mterm 'var (list (quote var-name) (quote var-sort)))))

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
    (pattern (op (op-name:id arg-sort:id ...+) range-sort:id)
             #:with ops
             #'(list (mterm 'op
                            (list (quote op-name)
                                  (mterm 'fixed-arity-domain
                                         (list (quote arg-sort)
                                               ...))
                                  (quote range-sort))))
             #:with rules #'(list))

    (pattern (=-> left:term right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (mterm '=->
                            (list (mterm 'vars empty) left.value right.value))))
    (pattern (=-> #:vars (var:variable ...) left:term right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (mterm '=->
                            (list (mterm 'vars (list var.var ...))
                                  left.value right.value))))
    (pattern (=-> #:var var:variable left:term right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (mterm '=->
                            (list (mterm 'vars (list var.var))
                                  left.value right.value))))
    (pattern (=-> left:term  #:if cond:term right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (mterm '=->?
                            (list (mterm 'vars empty)
                                  left.value cond.value right.value))))
    (pattern (=-> #:vars (var:variable ...)
                  left:term
                  #:if cond:term
                  right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (mterm '=->?
                            (list (mterm 'vars (list var.var ...))
                                  left.value cond.value right.value))))
    (pattern (=-> #:var var:variable
                  left:term 
                  #:if cond:term
                  right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (mterm '=->?
                            (list (mterm 'vars (list var.var))
                                  left.value cond.value right.value)))))
  
  (define-syntax-class import
    #:description "import"
    #:datum-literals (use extend)
    (pattern (use module:id)
             #:with value #'(mterm 'use
                                   (list (modules:module-hashcode module))))
    (pattern (extend module:id)
             #:with value #'(mterm 'extend
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

(define-syntax (meta-module* stx)
  (syntax-parse stx
    [(_ module-name:id
        import-decl:import ...
        sort-decl:sort ...
        op-decl:operator ...)
     #'(mterm 'module
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
                           (append op-decl.rules ...))))]))

(define-syntax (define-meta-module stx)
  (syntax-parse stx
    [(_ module-name:id decl ...)
     #'(define module-name (meta-module* module-name decl ...))]))

(define-syntax (define-module stx)
  (syntax-parse stx
    [(_ module-name:id decl ...)
     #'(define module-name
         (meta:meta-down meta:meta-module
                         (meta-module* module-name decl ...)))]))

(define-syntax (meta-term stx)
  (syntax-parse stx
    [(_  expr:term)
     #'expr.value]))

(define-syntax (term stx)
  (syntax-parse stx
    [(_ module:expr expr:term)
     #'(meta:meta-down module expr.value)]))
