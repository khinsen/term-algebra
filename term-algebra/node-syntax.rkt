#lang racket

(provide node define-node
         unchecked-node define-unchecked-node)

(require term-algebra/term-syntax
         term-algebra/library/node-transforms
         (prefix-in meta: term-algebra/meta)
         (prefix-in terms: term-algebra/terms)
         (only-in term-algebra/basic-api builtin:node)
         (for-syntax syntax/parse))

(define (prepare-import node . transforms)
  (cond
    [(empty? transforms) (meta:node-hashcode node)]
    [else
     (meta:node-hashcode
      (meta:reduce-vterm
       (term node-transforms
             (transformed-node ,node (transforms ,@transforms)))))]))

(define (combined-var-symbols node-var-symbols rule-var-symbols)
  (if (set-empty?
       (set-intersect node-var-symbols rule-var-symbols))
      (set-union node-var-symbols rule-var-symbols)
      (error "Redefinition of variable(s)")))

(define (used-node-vars left-pattern node-vars)

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
            node-vars)))

(begin-for-syntax

  (define-syntax-class node-transform
    #:description "node transform specification"
    #:attributes (transform)
    (pattern ((~datum node-name) name:id)
             #:with transform
             #'(term node-transforms
                     (node-name (quote name))))
    (pattern ((~datum add) import-decl:extended-import)
             #:with transform
             #'(term node-transforms
                     (add ,@import-decl.declarations)))
    (pattern ((~datum rename-sort) name1:id name2:id)
             #:with transform
             #'(term node-transforms
                     (rename-sort (quote name1) (quote name2))))
    (pattern ((~datum rename-op) name1:id name2:id)
             #:with transform
             #'(term node-transforms
                     (rename-op (quote name1) (quote name2)))))
  
  (define-syntax-class extended-import
    #:description "import declaration"
    #:datum-literals (use include)
    #:attributes (declarations)
    (pattern (use node:id
                  (~optional (~seq #:transforms mt:node-transform ...) 
                             #:defaults ([(mt.transform 1) null])) ...)
             #:with declarations
             #'(list (term builtin:node
                           (use ,(prepare-import node mt.transform ...)))))
    (pattern (include node:id
                      (~optional (~seq #:transforms mt:node-transform ...) 
                                 #:defaults ([(mt.transform 1) null])) ...)
             #:with declarations
             #'(list (term builtin:node
                           (include ,(prepare-import node mt.transform ...))))))
  
  (define-syntax-class sort
    #:description "sort"
    #:attributes (declarations)
    (pattern ((~datum sort) s-id:id)
             #:with declarations
             #'(list (term builtin:node (sorts (quote s-id)))))
    (pattern ((~datum sorts) s-id:id ...)
             #:with declarations
             #'(list (term builtin:node (sorts (quote s-id) ...))))
    (pattern ((~datum subsort) s-id1:id s-id2:id)
             #:with declarations
             #'(list (term builtin:node
                           (subsort (quote s-id1) (quote s-id2)))))
    (pattern ((~datum subsorts) [s-id1:id s-id2:id] ...)
             #:with declarations
             #'(list (term builtin:node
                           (subsort (quote s-id1) (quote s-id2))) ...)))
  
  (define-syntax-class operator
    #:description "operator"
    #:attributes (declarations)
    (pattern ((~datum op) op-name:id range-sort:id
              (~optional (~and #:symmetric (~bind [symmetric? #t]))))
             #:with declarations
             (if (attribute symmetric?)
                 #'(list (term builtin:node
                               (symop (quote op-name)
                                   (domain)
                                   (quote range-sort))))
                 #'(list (term builtin:node
                               (op (quote op-name)
                                   (domain)
                                   (quote range-sort))))))
    (pattern ((~datum op) (op-name:id arg-sort:id (~datum ...)) range-sort:id
              (~optional (~and #:symmetric (~bind [symmetric? #t]))))
             #:with declarations
             (if (attribute symmetric?)
                 #'(list (term builtin:node
                               (symop (quote op-name)
                                      (vl-domain (quote arg-sort))
                                      (quote range-sort))))
                 #'(list (term builtin:node
                               (op (quote op-name)
                                   (vl-domain (quote arg-sort))
                                   (quote range-sort))))))
    (pattern ((~datum op) (op-name:id arg-sort:id ...) range-sort:id
              (~optional (~and #:symmetric (~bind [symmetric? #t]))))
             #:with declarations
             (if (attribute symmetric?)
                 #'(list (term builtin:node
                               (symop (quote op-name)
                                      (domain (quote arg-sort) ...)
                                      (quote range-sort))))
                 #'(list (term builtin:node
                               (op (quote op-name)
                                   (domain (quote arg-sort) ...)
                                   (quote range-sort)))))))

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
                   (term builtin:node (var-ref (quote symbol)))
                   (term builtin:node (term (quote symbol) (args)))))
    (pattern (symbol:id (~var arg-terms (term-pattern var-symbols)) ...)
             #:with value
             #'(term builtin:node
                     (pattern (quote symbol)
                              (args ,arg-terms.value ...)))))

  (define-syntax-class variable
    #:description "variable in rule"
    #:attributes (var var-symbol)
    (pattern [var-symbol:id sort-symbol:id]
             #:with var
             #'(term builtin:node
                     (var (quote var-symbol) (quote sort-symbol))))
    (pattern [var-symbol:id sort-symbol:id (~datum ...)]
             #:with var
             #'(term builtin:node
                     (svar (quote var-symbol) (quote sort-symbol) false)))
    (pattern [var-symbol:id sort-symbol:id (~datum ?...)]
             #:with var
             #'(term builtin:node
                     (svar (quote var-symbol) (quote sort-symbol) true))))

  (define-splicing-syntax-class variable-list
    #:description "variable list in a rule"
    #:attributes (value symbols)
    (pattern (~seq #:var v:variable)
             #:with value #'(list v.var)
             #:with symbols #'(set (quote v.var-symbol)))
    (pattern (~seq #:vars (v:variable ...))
             #:with value #'(list v.var ...)
             #:with symbols #'(set (quote v.var-symbol) ...)))

  (define-syntax-class eq-or-rule
    #:description "equation/rule"
    #:attributes (declarations mvars)
    (pattern ((~datum vars) v:variable ...)
             #:with declarations #'empty
             #:with mvars
             #'(if (ormap (λ (s) (set-member? node-var-symbols s))
                          (list (quote v.var-symbol) ...))
                   (error "Redefinition of variable(s)")
                   (set!-values
                    (node-vars node-var-symbols)
                    (values (append (list v.var ...)
                                    node-vars)
                            (set-union (set (quote v.var-symbol) ...)
                                       node-var-symbols)))))
    (pattern ((~datum =>)
              (~optional rule-vars:variable-list
                         #:defaults ([rule-vars.value #'empty]
                                     [rule-vars.symbols #'(set)]))
              (~var left (term-pattern #'var-symbols))
              (~optional (~seq #:if (~var cond (term-pattern #'var-symbols)))
                         #:defaults ([cond.value #'(term builtin:node no-condition)]))
              (~var right (term-pattern #'var-symbols)))
             #:with declarations
             #'(let* ([var-symbols (combined-var-symbols
                                    node-var-symbols rule-vars.symbols)]
                      [used-vars (used-node-vars left.value node-vars)])
                 (list (term builtin:node
                             (=> (vars ,@used-vars ,@rule-vars.value)
                                 ,left.value ,right.value ,cond.value))))
             #:with mvars #'(void))
    (pattern ((~datum =>)
              (~optional rule-vars:variable-list
                         #:defaults ([rule-vars.value #'empty]
                                     [rule-vars.symbols #'(set)]))
              (~var left (term-pattern #'var-symbols))
              (~seq #:cond [(~var cond (term-pattern #'var-symbols))
                            (~var right (term-pattern #'var-symbols))] ...
                            [#:else (~var else (term-pattern #'var-symbols))]))
             #:with declarations
             #'(let* ([var-symbols (combined-var-symbols
                                    node-var-symbols rule-vars.symbols)]
                      [used-vars (used-node-vars left.value node-vars)])
                 (list (term builtin:node
                             (=> (vars ,@used-vars ,@rule-vars.value)
                                 ,left.value ,right.value ,cond.value))
                       ...
                       (term builtin:node
                             (=> (vars ,@used-vars ,@rule-vars.value)
                                 ,left.value ,else.value no-condition))))
             #:with mvars #'(void))

    (pattern ((~datum =)
              (~optional rule-vars:variable-list
                         #:defaults ([rule-vars.value #'empty]
                                     [rule-vars.symbols #'(set)]))
              (~var left (term-pattern #'var-symbols))
              (~var right (term-pattern #'var-symbols))
              (~optional (~seq #:if (~var cond (term-pattern #'var-symbols)))
                         #:defaults ([cond.value #'(term builtin:node no-condition)])))
             #:with declarations
             #'(let* ([var-symbols (combined-var-symbols
                                    node-var-symbols rule-vars.symbols)]
                      [used-vars (used-node-vars left.value node-vars)])
                 (list (term builtin:node
                             (= (vars ,@used-vars ,@rule-vars.value)
                                ,left.value ,right.value ,cond.value))))
             #:with mvars #'(void)))

  (define-syntax-class decl
    #:description "declaration in a node"
    #:attributes (declarations mvars)
    (pattern import-decl:extended-import
             #:with declarations #'import-decl.declarations
             #:with mvars #'(void))
    (pattern sort-decl:sort
             #:with declarations  #'sort-decl.declarations
             #:with mvars #'(void))
    (pattern op-decl:operator
             #:with declarations #'op-decl.declarations
             #:with mvars #'(void))
    (pattern eq-or-rule-decl:eq-or-rule
             #:with declarations #'eq-or-rule-decl.declarations
             #:with mvars #'eq-or-rule-decl.mvars)))

(define-syntax (unchecked-node stx)
  (syntax-parse stx
    [(_ node-name:id
        declaration:decl ...)
     #'(let ([node-vars empty]
             [node-var-symbols (set)])
         declaration.mvars ...
         (term extended-node
               (node (quote node-name)
                     (declarations
                      ,@(append declaration.declarations ...)))))]))

(define-syntax (define-unchecked-node stx)
  (syntax-parse stx
    [(_ node-name:id decl ...)
     #'(define node-name (unchecked-node node-name decl ...))]))

(define-syntax (node stx)
  (syntax-parse stx
    [(_ arg ...)
     #'(meta:check-node (meta:reduce-vterm (unchecked-node arg ...)))]))

(define-syntax (define-node stx)
  (syntax-parse stx
    [(_ node-name:id decl ...)
     #'(define node-name (node node-name decl ...))]))
