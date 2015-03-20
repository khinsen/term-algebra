#lang racket

(provide define-module module
         define-unchecked-module unchecked-module
         m-module)

(require term-algebra/term-syntax
         (prefix-in terms: term-algebra/terms)
         (prefix-in meta: term-algebra/meta)
         (for-syntax syntax/parse))

(define m-module
  (meta:check-module
   (meta:make-vterm meta:m-module
                    (terms:term 'builtin-module (list 'm-module) 'Module))))

(begin-for-syntax

  (define-syntax-class import
    #:description "import"
    #:attributes (imports sorts subsorts ops rules)
    (pattern ((~datum use) module:id)
             #:with imports
             #'(list (term m-module
                           (use ,(meta:module-hashcode module))))
             #:with sorts #'empty
             #:with subsorts #'empty
             #:with ops #'empty
             #:with rules #'empty)
    (pattern ((~datum include) module:id)
             #:with imports
             #'(list (term m-module
                           (include ,(meta:module-hashcode module))))
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
             #:with value #'(term m-module (vars ,v.var))
             #:with symbols #'(set (quote v.var-symbol)))
    (pattern (~seq #:vars (v:variable ...))
             #:with value #'(term m-module (vars ,v.var ...))
             #:with symbols #'(set (quote v.var-symbol) ...)))

  (define-syntax-class operator
    #:description "operator/rule"
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
             #:with subsorts #'empty)

    (pattern ((~datum =->)
              (~optional vars:variable-list
                         #:defaults ([vars.value #'(term m-module (vars))]
                                     [vars.symbols #'(set)]))
              (~var left (term-pattern #'var-symbols))
              (~optional (~seq #:if (~var cond (term-pattern #'var-symbols)))
                         #:defaults ([cond.value #'(term m-module no-condition)]))
              (~var right (term-pattern #'var-symbols)))
             #:with ops #'empty
             #:with rules
             #'(let ([var-symbols vars.symbols])
                 (list (term m-module
                             (=-> ,vars.value
                                  ,left.value ,cond.value ,right.value))))
             #:with imports #'empty
             #:with sorts #'empty
             #:with subsorts #'empty)))

(define-syntax (unchecked-module stx)
  (syntax-parse stx
    [(_ module-name:id
        import-decl:import ...
        sort-decl:sort ...
        op-decl:operator ...)
     #'(term m-module
             (module (quote module-name)
               (imports ,@(append import-decl.imports ...))
               (sorts ,@(append sort-decl.sorts ...))
               (subsorts ,@(append sort-decl.subsorts ...))
               (ops ,@(append op-decl.ops ...))
               (rules ,@(append op-decl.rules ...))))]))

(define-syntax (define-unchecked-module stx)
  (syntax-parse stx
    [(_ module-name:id decl ...)
     #'(define module-name (unchecked-module module-name decl ...))]))

(define-syntax (module stx)
  (syntax-parse stx
    [(_ arg ...)
     #'(meta:check-module (unchecked-module arg ...))]))

(define-syntax (define-module stx)
  (syntax-parse stx
    [(_ module-name:id decl ...)
     #'(define module-name (module module-name decl ...))]))
