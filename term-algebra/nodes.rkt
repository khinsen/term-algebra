#lang racket

(provide (struct-out node)
         define-builtin-node
         lookup-node-hash
         make-node
         make-term
         imports?)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in terms: term-algebra/terms)
         (prefix-in rules: term-algebra/rules)
         (for-syntax syntax/parse))

;
; The data structure for the internal node representation
;
(struct node (name ops rules imports hashcode)
        #:transparent)

;
; The node registry
;
(define *nodes* (make-hash))

(define (register-node node)
  (hash-set! *nodes* (node-hashcode node) (make-weak-box node)))

(define (lookup-node-hash hash)
  (weak-box-value (hash-ref *nodes* hash (make-weak-box #f))))

;
; Macro for defining builtin nodes
;
(define-syntax (define-builtin-node stx)

  (define-syntax-class import-builtin
    #:description "import for builtin nodes"
    #:attributes (node use)
    (pattern ((~datum use) node:id)
             #:with use #'#t)
    (pattern ((~datum include) node:id)
             #:with use #'#f))

  (define-syntax-class op-builtin
    #:description "operator/rule for builtin nodes"
    #:attributes (ops fns)

    (pattern ((~datum op) op-name:id range-sort:id)
             #:with ops
             #'(list (list (quote op-name)
                           '()
                           (quote range-sort)
                           (set)))
             #:with fns #'(list))
    (pattern ((~datum op)
              (op-name:id arg-sort:id (~datum ...))
              range-sort:id
              (~optional (~and #:symmetric (~bind [symmetric? #t]))))
             #:with ops
             (if (attribute symmetric?)
                 #'(list (list (quote op-name)
                               (list (quote arg-sort))
                               (quote range-sort)
                               (set 'var-arity 'symmetric)))
                 #'(list (list (quote op-name)
                               (list (quote arg-sort))
                               (quote range-sort)
                               (set 'var-arity))))
             #:with fns #'(list))
    (pattern ((~datum op) (op-name:id arg-sort:id ...) range-sort:id)
             #:with ops
             #'(list (list (quote op-name)
                           (list (quote arg-sort) ...)
                           (quote range-sort)
                           (set)))
             #:with fns #'(list))
    (pattern ((~datum fn) op-name:id proc:expr)
             #:with ops #'(list)
             #:with fns #'(list (list (quote op-name) proc))))

  (syntax-parse stx
    [(_ node-name:id
        import-decl:import-builtin ...
        (~optional
         ((~datum sorts) s-id:id ...))
        (~optional
         ((~datum subsorts) (s-id1:id s-id2:id) ...))
        (~optional
         ((~datum special-ops) s-op:id ...))
        op-decl:op-builtin ...)
     (with-syntax ([import-list (if (attribute import-decl.node)
                                    #'(map (lambda (a b) (cons a b))
                                           (list import-decl.node ...)
                                           (list import-decl.use ...))
                                    #'(list))]
                   [sort-list (if (attribute s-id)
                                  #'(list (quote s-id) ...)
                                  #'(list))]
                   [subsort-list (if (attribute s-id1)
                                     #'(list (cons (quote s-id1)
                                                   (quote s-id2))
                                             ...)
                                     #'(list))]
                   [special-op-set (if (attribute s-op)
                                       #'(set (quote s-op) ...)
                                       #'(set))]
                   [op-list (if (attribute op-decl.ops)
                                #'(append op-decl.ops ...)
                                #'(list))]
                   [s-op-list (if (attribute s-op)
                                  #'(list (quote s-op) ...)
                                  #'(list))]
                   [fn-list (if (attribute op-decl.fns)
                                #'(append op-decl.fns ...)
                                #'(list))]
                   [make-term (datum->syntax stx 'make-term)]
                   [is-sort? (datum->syntax stx 'is-sort?)])
       #'(define node-name
           (letrec ([make-term
                     (λ (op args) (terms:make-term op args (node-ops node)))]
                    [is-sort?
                     (λ (term target-sort)
                       (sorts:is-sort? (terms:sort-of term) target-sort
                                       (operators:op-set-sorts
                                        (node-ops node))))]
                    [node
                     (make-node (quote node-name)
                      ; We can't construct a meta-term here because
                      ; the meta module is not yet available. We cheat
                      ; by using the textual representation of the term.
                      ; It is used only to compute the hashcode.
                      (format "(builtin-node ~s)" (quote node-name))
                      import-list sort-list subsort-list
                      op-list s-op-list fn-list #t)])
             node)))]))

(define (make-node node-name meta-terms
                   import-list sort-list subsort-list
                   op-list s-op-list fn-list
                   strict-checking)
  ; strict-checking enables checks for situations that are
  ; usually errors in hand-written nodes but can occur in
  ; metaprogramming: redefinitions of sorts, subsorts, and operator
  ; signatures, and declaring a sort its own subsort.

  (define (merge-imports imports1 imports2)
    (for/fold ([imports imports1])
              ([kv (hash->list imports2)])
      (hash-set imports (car kv) (cdr kv))))

  (define hashcode (terms:term-hashcode meta-terms))

  (or (lookup-node-hash hashcode)
      (let*-values
          ; imports is a list of (hashcode restricted-mode) sublists
          ([(imports) (for/list ([import-spec import-list])
                        (match-let ([(cons mod rmode) import-spec])
                          (list mod
                                (hash-set
                                 (if rmode
                                     (for/hash ([(hashcode _)
                                                 (in-hash (node-imports mod))])
                                       (values hashcode #t))
                                     (node-imports mod))
                                 (node-hashcode mod) rmode))))]
           ; all-imports maps hashcodes to a boolean indicating restricted mode
           [(all-imports) (for/fold ([all-imports (hash)])
                                    ([import imports])
                            (merge-imports all-imports (second import)))]
           ; The sort graph is constructed from (1) imports, (2) sorts,
           ; (3) subsorts.
           [(sorts _) (for/fold ([sorts (sorts:empty-sort-graph)]
                                 [prior-imports (hash)])
                                ([import imports])
                        (values (sorts:merge-sort-graph
                                 (operators:op-set-sorts
                                  (node-ops (first import)))
                                 prior-imports
                                 sorts)
                                (merge-imports prior-imports (second import))))]
           [(sorts) (for/fold ([sorts sorts])
                              ([sort sort-list])
                      (sorts:add-sort sort hashcode all-imports sorts
                                      strict-checking))]
           [(sorts) (for/fold ([sorts sorts])
                              ([sort-pair subsort-list])
                      (sorts:add-subsort (car sort-pair) (cdr sort-pair)
                                         hashcode all-imports sorts
                                         strict-checking))]
           ; The operator signatures are constructed from (1) imports
           ; (2) op declarations (3) special ops.
           [(ops _) (for/fold ([ops (operators:empty-op-set sorts)]
                               [prior-imports (hash)])
                              ([import imports])
                      (values (operators:merge-op-set
                               (node-ops (first import))
                               prior-imports
                               ops)
                              (merge-imports prior-imports (second import))))]
           [(ops) (for/fold ([ops ops])
                            ([op-spec op-list])
                    (match-let ([(list symbol domain range properties)
                                 op-spec])
                      (operators:add-op symbol domain range
                                        properties hashcode all-imports ops
                                        strict-checking)))]
           [(ops) (for/fold ([ops ops])
                            ([s-op-symbol s-op-list])
                    (operators:add-special-op s-op-symbol hashcode
                                              all-imports ops))]
           ; Rules are initialized to an empty mutable hash. This is required
           ; for constructing nodes from their meta-representation.
           [(rules) (rules:empty-rules)])
        ; Rules are constructed from (1) imports (2) procedure specifications.
        ; Part (2) is effective only for builtin nodes. Meta-nodes are
        ; instantiated with an empty rule set at this stage.
        (for/fold ([prior-imports (hash)])
                  ([import imports])
          (rules:merge-rules! (node-rules (first import))
                              prior-imports
                              rules)
          (merge-imports prior-imports (second import)))
        (for ([fn-spec fn-list])
          (match-let ([(list symbol proc-expr) fn-spec])
            (rules:add-proc! symbol proc-expr hashcode rules)))
        (let ([mod (node node-name ops rules all-imports hashcode)])
          (register-node mod)
          mod))))

; Make a term relative to a node
; (used in rewrite.rkt)
(define (make-term op args node)
  (terms:make-term op args (node-ops node)))

; Check if a node imports another one

(define (imports? mod-a mod-b)
  (or (equal? mod-a mod-b)
      (hash-has-key? (node-imports mod-a) (node-hashcode mod-b))))
