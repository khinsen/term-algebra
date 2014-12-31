#lang racket

(provide define-module define-meta-module
         term meta-term)

(require (prefix-in modules: term-algebra/modules)
         (prefix-in builtin: term-algebra/builtin)
         (for-syntax syntax/parse))

(define op-module (modules:op-from builtin:meta-module 'module))

(define op-use (modules:op-from builtin:meta-module 'use))
(define op-extend (modules:op-from builtin:meta-module 'extend))
  
(define op-subsort (modules:op-from builtin:meta-module 'subsort))
  
(define op-op (modules:op-from builtin:meta-module 'op))
(define op-domain (modules:op-from builtin:meta-module 'domain))
(define op-var-length-domain (modules:op-from builtin:meta-module
                                              'var-length-domain))
  
(define op-eqrule (modules:op-from builtin:meta-module '=->))
(define op-ceqrule (modules:op-from builtin:meta-module '=->?))
(define op-vars (modules:op-from builtin:meta-module 'vars))
(define op-var (modules:op-from builtin:meta-module 'var))
(define op-term (modules:op-from builtin:meta-module 'term))

(begin-for-syntax

  (define-syntax-class term
    #:description "term"
    (pattern symbol:id
             #:with value
             #'(modules:make-mterm op-term
                                   (list (quote symbol))))
    (pattern s:str #:with value #'s)
    (pattern ((~literal quote) symbol:id)
             #:with value #'(quote symbol))
    (pattern (op:id args:term ...)
             #:with value
             #'(modules:make-mterm op-term
                                   (list (quote op) args.value ...)))
    (pattern x:number #:when (exact? (syntax-e #'x))
             #:with value #'x))

  (define-syntax-class variable
    #:description "variable in rule"
    #:attributes (var)
    (pattern [var-name:id var-sort:id]
             #:with var
             #'(modules:make-mterm op-var (list (quote var-name)
                                                (quote var-sort)))))

  (define-syntax-class operator
    #:description "operator/rule"
    #:attributes (ops rules)
    #:datum-literals (op =->)

    (pattern (op op-name:id range-sort:id)
             #:with ops
             #'(list (modules:make-mterm op-op
                       (list (quote op-name)
                             (modules:make-mterm op-domain
                                                 (list))
                             (quote range-sort))))
             #:with rules #'(list))
    (pattern (op (op-name:id arg-sort:id ...+ (~datum ...)) range-sort:id)
             #:with ops
             #'(list (modules:make-mterm op-op
                       (list (quote op-name)
                             (modules:make-mterm op-var-length-domain
                                                 (list (quote
                                                        arg-sort)
                                                       ...))
                             (quote range-sort))))
             #:with rules #'(list))
    (pattern (op (op-name:id arg-sort:id ...+) range-sort:id)
             #:with ops
             #'(list (modules:make-mterm op-op
                       (list (quote op-name)
                             (modules:make-mterm op-domain
                                                 (list (quote
                                                        arg-sort)
                                                       ...))
                             (quote range-sort))))
             #:with rules #'(list))

    (pattern (=-> left:term right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (modules:make-mterm op-eqrule
                       (list (modules:make-mterm op-vars '())
                             left.value right.value))))
    (pattern (=-> #:vars (var:variable ...) left:term right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (modules:make-mterm op-eqrule
                       (list (modules:make-mterm op-vars
                                                 (list var.var
                                                       ...))
                             left.value right.value))))
    (pattern (=-> #:var var:variable left:term right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (modules:make-mterm op-eqrule
                       (list (modules:make-mterm op-vars
                                                 (list var.var))
                             left.value right.value))))
    (pattern (=-> left:term  #:if cond:term right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (modules:make-mterm op-ceqrule
                       (list (modules:make-mterm op-vars '())
                             left.value
                             cond.value
                             right.value))))
    (pattern (=-> #:vars (var:variable ...)
                  left:term
                  #:if cond:term
                  right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (modules:make-mterm op-ceqrule
                       (list (modules:make-mterm op-vars
                                                 (list var.var
                                                       ...))
                             left.value
                             cond.value
                             right.value))))
    (pattern (=-> #:var var:variable
                  left:term 
                  #:if cond:term
                  right:term)
             #:with ops #'(list)
             #:with rules
             #'(list (modules:make-mterm op-ceqrule
                       (list (modules:make-mterm op-vars
                                                 (list var.var))
                             left.value
                             cond.value
                             right.value)))))
  
  (define-syntax-class import
    #:description "import"
    #:datum-literals (use extend)
    (pattern (use module:id)
             #:with value #'(modules:make-mterm
                             op-use (list (modules:module-meta-hash module))))
    (pattern (extend module:id)
             #:with value #'(modules:make-mterm
                             op-extend (list (modules:module-meta-hash module)))))
  
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
             #'(list (modules:make-mterm op-subsort
                                         (list (quote s-id1)
                                               (quote s-id2)))))
    (pattern ((~datum subsorts) [s-id1:id s-id2:id] ...)
             #:with sorts #'(list)
             #:with subsorts
             #'(list (modules:make-mterm op-subsort
                                         (list (quote s-id1)
                                               (quote s-id2))) ...))))

(define-syntax (define-meta-module stx)
  (syntax-parse stx
    [(_ module-name:id
        import-decl:import ...
        sort-decl:sort ...
        op-decl:operator ...)
     #'(define module-name
         (modules:meta-module (quote module-name)
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
         (modules:module-from-meta
          (modules:meta-module (quote module-name)
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
     #'(modules:term-from-meta
        (modules:module-sorts module)
        (modules:module-ops module)
        (hash)
        expr.value)]))
