#lang racket

(provide define-node node
         define-unchecked-node unchecked-node
         n-node
         (for-syntax import sort operator))

(require term-algebra/term-syntax
         (prefix-in terms: term-algebra/terms)
         (prefix-in meta: term-algebra/meta)
         (for-syntax syntax/parse))

(define n-node
  (meta:check-node
   (meta:make-vterm meta:n-node
                    (terms:term 'builtin-node (list 'node) 'Node))))

(begin-for-syntax

  (define-syntax-class import
    #:description "import"
    #:attributes (imports sorts subsorts ops rules)
    (pattern ((~datum use) node:id)
             #:with imports
             #'(list (term n-node
                           (use ,(meta:node-hashcode node))))
             #:with sorts #'empty
             #:with subsorts #'empty
             #:with ops #'empty
             #:with rules #'empty)
    (pattern ((~datum include) node:id)
             #:with imports
             #'(list (term n-node
                           (include ,(meta:node-hashcode node))))
             #:with sorts #'empty
             #:with subsorts #'empty
             #:with ops #'empty
             #:with rules #'empty))
  
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
             #'(list (term n-node
                           (subsort (quote s-id1) (quote s-id2))))
             #:with imports #'empty
             #:with ops #'empty
             #:with rules #'empty)
    (pattern ((~datum subsorts) [s-id1:id s-id2:id] ...)
             #:with sorts #'empty
             #:with subsorts
             #'(list (term n-node
                           (subsort (quote s-id1) (quote s-id2))) ...)
             #:with imports #'empty
             #:with ops #'empty
             #:with rules #'empty))
  
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
                   (term n-node (var-ref (quote symbol)))
                   (term n-node (term (quote symbol) (args)))))
    (pattern (symbol:id (~var arg-terms (term-pattern var-symbols)) ...)
             #:with value
             #'(term n-node
                     (pattern (quote symbol)
                              (args ,arg-terms.value ...)))))

  (define-syntax-class variable
    #:description "variable in rule"
    #:attributes (var var-symbol)
    (pattern [var-symbol:id sort-symbol:id]
             #:with var
             #'(term n-node
                     (var (quote var-symbol) (quote sort-symbol))))
    (pattern [var-symbol:id sort-symbol:id (~datum ...)]
             #:with var
             #'(term n-node
                     (svar (quote var-symbol) (quote sort-symbol)))))

  (define-splicing-syntax-class variable-list
    #:description "variable list in a rule"
    #:attributes (value symbols)
    (pattern (~seq #:var v:variable)
             #:with value #'(term n-node (vars ,v.var))
             #:with symbols #'(set (quote v.var-symbol)))
    (pattern (~seq #:vars (v:variable ...))
             #:with value #'(term n-node (vars ,v.var ...))
             #:with symbols #'(set (quote v.var-symbol) ...)))

  (define-syntax-class operator
    #:description "operator/rule"
    #:attributes (imports sorts subsorts ops rules)
    (pattern ((~datum op) op-name:id range-sort:id
              (~optional (~and #:symmetric (~bind [symmetric? #t]))))
             #:with ops
             (if (attribute symmetric?)
                 #'(list (term n-node
                               (symop (quote op-name)
                                      (domain)
                                      (quote range-sort))))
                 #'(list (term n-node
                               (op (quote op-name)
                                   (domain)
                                   (quote range-sort)))))
             #:with rules #'empty
             #:with imports #'empty
             #:with sorts #'empty
             #:with subsorts #'empty)
    (pattern ((~datum op) (op-name:id arg-sort:id (~datum ...))
              range-sort:id
              (~optional (~and #:symmetric (~bind [symmetric? #t]))))
             #:with ops
             (if (attribute symmetric?)
                 #'(list (term n-node
                               (symop (quote op-name)
                                      (vl-domain (quote arg-sort))
                                      (quote range-sort))))
                 #'(list (term n-node
                               (op (quote op-name)
                                   (vl-domain (quote arg-sort))
                                   (quote range-sort)))))
             #:with rules #'empty
             #:with imports #'empty
             #:with sorts #'empty
             #:with subsorts #'empty)
    (pattern ((~datum op) (op-name:id arg-sort:id ...) range-sort:id)
             #:with ops
             #'(list (term n-node
                           (op (quote op-name)
                               (domain (quote arg-sort) ...)
                               (quote range-sort))))
             #:with rules #'empty
             #:with imports #'empty
             #:with sorts #'empty
             #:with subsorts #'empty)

    (pattern ((~datum =->)
              (~optional vars:variable-list
                         #:defaults ([vars.value #'(term n-node (vars))]
                                     [vars.symbols #'(set)]))
              (~var left (term-pattern #'var-symbols))
              (~optional (~seq #:if (~var cond (term-pattern #'var-symbols)))
                         #:defaults ([cond.value #'(term n-node no-condition)]))
              (~var right (term-pattern #'var-symbols)))
             #:with ops #'empty
             #:with rules
             #'(let ([var-symbols vars.symbols])
                 (list (term n-node
                             (=-> ,vars.value
                                  ,left.value ,cond.value ,right.value))))
             #:with imports #'empty
             #:with sorts #'empty
             #:with subsorts #'empty)))

(define-syntax (unchecked-node stx)
  (syntax-parse stx
    [(_ node-name:id
        import-decl:import ...
        sort-decl:sort ...
        op-decl:operator ...)
     #'(term n-node
             (node (quote node-name)
               (imports ,@(append import-decl.imports ...))
               (sorts ,@(append sort-decl.sorts ...))
               (subsorts ,@(append sort-decl.subsorts ...))
               (ops ,@(append op-decl.ops ...))
               (rules ,@(append op-decl.rules ...))))]))

(define-syntax (define-unchecked-node stx)
  (syntax-parse stx
    [(_ node-name:id decl ...)
     #'(define node-name (unchecked-node node-name decl ...))]))

(define-syntax (node stx)
  (syntax-parse stx
    [(_ arg ...)
     #'(meta:check-node (unchecked-node arg ...))]))

(define-syntax (define-node stx)
  (syntax-parse stx
    [(_ node-name:id decl ...)
     #'(define node-name (node node-name decl ...))]))
