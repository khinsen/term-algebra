#lang racket

(provide define-module define-meta-module
         term meta-term)

(require (prefix-in modules: term-algebra/modules)
         (prefix-in terms: term-algebra/terms)
         (prefix-in meta: term-algebra/meta)
         (for-syntax syntax/parse))

(define meta-term-ops (modules:module-ops meta:meta-term))
(define meta-pattern-ops (modules:module-ops meta:meta-pattern))
(define meta-module-ops (modules:module-ops meta:meta-module))

(define (mterm op args)
  (terms:make-term op args meta-module-ops))

(define (pterm op args)
  (terms:make-term op args meta-pattern-ops))

(define (tterm op args)
  (terms:make-term op args meta-term-ops))

(define no-condition (mterm 'no-condition empty))

(define (fix-var-refs rule)

  (define (fix-var-refs* var-symbols term)
    (if (and (terms:term? term)
             (member (terms:term-op term) '(term pattern)))
        (let ([t-args (terms:term-args term)])
          (if (and (member (first t-args) var-symbols)
                   (equal? (second t-args) (tterm 'args empty)))
              (mterm 'var-ref (list (first t-args)))
              (mterm (terms:term-op term)
               (match-let ([(list op args) t-args])
                 (list op
                       (cond
                         [(equal? (terms:term-op args) 'head-tail)
                          (mterm 'head-tail
                                 (map (λ (a) (fix-var-refs* var-symbols a))
                                      (terms:term-args args)))]
                         [(equal? (terms:term-op args) 'args)
                          (mterm 'args
                                 (map (λ (a) (fix-var-refs* var-symbols a))
                                      (terms:term-args args)))]
                         [else (error "unknown arglist type")]))))))
        term))

  (match-let ([(list vars left condition right) (terms:term-args rule)])
    (let ([var-symbols (map (λ (vt) (first (terms:term-args vt)))
                            (terms:term-args vars))])
      (mterm (terms:term-op rule) (list vars
                                        (fix-var-refs* var-symbols left)
                                        (fix-var-refs* var-symbols condition)
                                        (fix-var-refs* var-symbols right))))))


(begin-for-syntax

  (define-syntax-class term
    #:description "term"
    (pattern symbol:id
             #:with value
             #'(tterm 'term (list (quote symbol)
                                  (tterm 'args (list)))))
    (pattern s:str #:with value #'s)
    (pattern ((~literal quote) symbol:id)
             #:with value #'(quote symbol))
    (pattern (symbol:id arg-terms:term ...)
             #:with value
             #'(tterm 'term (list (quote symbol)
                                  (tterm 'args (list arg-terms.value ...)))))
    (pattern x:number #:when (exact? (syntax-e #'x))
             #:with value #'x))

  (define-syntax-class term-pattern
    #:description "term pattern"
    (pattern (symbol:id head:term-pattern (~datum :) tail:term-pattern)
             #:with value
             #'(pterm 'pattern (list (quote symbol)
                                     (pterm 'head-tail (list head.value
                                                             tail.value)))))
    (pattern (symbol:id arg-terms:term-pattern ...)
             #:with value
             #'(pterm 'pattern (list (quote symbol)
                                     (pterm 'args (list arg-terms.value ...)))))
    (pattern t:term
             #:with value #'t.value))

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
    (pattern (op (op-name:id arg-sort:id ...) range-sort:id)
             #:with ops
             #'(list (mterm 'op
                            (list (quote op-name)
                                  (mterm 'fixed-arity-domain
                                         (list (quote arg-sort)
                                               ...))
                                  (quote range-sort))))
             #:with rules #'(list))

    (pattern (=-> left:term-pattern right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (mterm '=->
                            (list (mterm 'vars empty)
                                  left.value no-condition right.value))))
    (pattern (=-> #:vars (var:variable ...) left:term-pattern right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (mterm '=->
                            (list (mterm 'vars (list var.var ...))
                                  left.value no-condition right.value))))
    (pattern (=-> #:var var:variable left:term-pattern right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (mterm '=->
                            (list (mterm 'vars (list var.var))
                                  left.value no-condition right.value))))
    (pattern (=-> left:term-pattern  #:if cond:term right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (mterm '=->
                            (list (mterm 'vars empty)
                                  left.value cond.value right.value))))
    (pattern (=-> #:vars (var:variable ...)
                  left:term-pattern
                  #:if cond:term
                  right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (mterm '=->
                            (list (mterm 'vars (list var.var ...))
                                  left.value cond.value right.value))))
    (pattern (=-> #:var var:variable
                  left:term-pattern
                  #:if cond:term
                  right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (mterm '=->
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
                           (map fix-var-refs (append op-decl.rules ...)))))]))

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
     #'(modules:make-vterm meta:meta-term expr.value)]))

(define-syntax (term stx)
  (syntax-parse stx
    [(_ module:expr expr:term)
     #'(modules:make-vterm module (meta:meta-down module expr.value))]))
