#lang racket

(provide (struct-out module)
         (struct-out vterm)
         define-builtin-module
         module-hash lookup-module-hash
         make-module
         make-term make-vterm)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in terms: term-algebra/terms)
         (prefix-in rules: term-algebra/rules)
         (for-syntax syntax/parse)
         (only-in file/sha1 sha1))

;
; The data structure for the internal module representation
;
(struct module (name ops rules imports meta hashcode)
        #:transparent)

;
; The data structure for a term validated with reference to a module
;
(struct vterm (module sort term)
        #:transparent
        #:property prop:custom-write
        (lambda (vterm port mode)
          (define (write-term term op-set port)
            (let ([op (terms:term-op term)])
              (if (and (null? (terms:term-args term))
                       (not (operators:has-var-arity? op op-set)))
                  (write op port)
                  (write (cons op (terms:term-args term)) port))))
          (let* ([mod (vterm-module vterm)]
                 [ops (module-ops mod)]
                 [term (vterm-term vterm)])
            (write (module-name mod) port)
            (write-string ":" port)
            (write (vterm-sort vterm) port)
            (write-string ":" port)
            (if (terms:term? term)
                (write-term term ops port)
                (write term port)))))

;
; The module registry
;
(define (module-hash module-name meta-module)

  (define (hash-of-string s)
    (sha1 (open-input-string s)))

  (if meta-module
      (let ([o (open-output-string)])
        (write meta-module o)
        (hash-of-string (get-output-string o)))
      (hash-of-string (symbol->string module-name))))

(define *modules* (make-hash))

(define (register-module module)
  (hash-set! *modules* (module-hashcode module) (make-weak-box module)))

(define (lookup-module-hash hash)
  (weak-box-value (hash-ref *modules* hash)))

(define (make-term op args module)
  (terms:make-term op args (module-ops module)))

(define (make-vterm module term)
  (vterm module (terms:sort-of term) term))

;
; Macro for defining builtin modules
;
(define-syntax (define-builtin-module stx)

  (define-syntax-class import-builtin
    #:description "import for builtin modules"
    #:attributes (module use)
    (pattern ((~datum use) module:id)
             #:with use #'#t)
    (pattern ((~datum include) module:id)
             #:with use #'#f))

  (define-syntax-class op-builtin
    #:description "operator/rule for builtin modules"
    #:attributes (ops fns)

    (pattern ((~datum op) op-name:id range-sort:id)
             #:with ops
             #'(list (list (quote op-name)
                           '()
                           (quote range-sort)
                           (set)))
             #:with fns #'(list))
    (pattern ((~datum op)
              (op-name:id arg-sort:id ...+ (~datum ...))
              range-sort:id)
             #:with ops
             #'(list (list (quote op-name)
                           (list (quote arg-sort) ...)
                           (quote range-sort)
                           (set 'var-arity)))
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
    [(_ module-name:id
        import-decl:import-builtin ...
        (~optional
         ((~datum sorts) s-id:id ...))
        (~optional
         ((~datum subsorts) (s-id1:id s-id2:id) ...))
        (~optional
         ((~datum special-ops) s-op:id ...))
        op-decl:op-builtin ...)
     (with-syntax ([import-list (if (attribute import-decl.module)
                                    #'(map (lambda (a b) (cons a b))
                                           (list import-decl.module ...)
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
                                #'(list))])
       #'(define module-name
           (make-module (quote module-name) #f
                        import-list sort-list subsort-list
                        op-list s-op-list fn-list)))]))

(define (make-module module-name meta-terms
                     import-list sort-list subsort-list
                     op-list s-op-list fn-list)

  (define (merge-imports imports1 imports2)
    (for/fold ([imports imports1])
              ([kv (hash->list imports2)])
      (hash-set imports (car kv) (cdr kv))))

  (let*-values
      ([(imports) (for/list ([import-spec import-list])
                    (match-let ([(cons mod rmode) import-spec])
                      (list mod
                            (hash-set
                             (if rmode
                                 (for/hash ([(hashcode _)
                                             (in-hash (module-imports mod))])
                                   (values hashcode #t))
                                 (module-imports mod))
                             (module-hashcode mod) rmode))))]
       [(all-imports) (for/fold ([all-imports (hash)])
                                ([import imports])
                        (merge-imports all-imports (second import)))]
       [(hashcode) (module-hash module-name meta-terms)]
       [(sorts _) (for/fold ([sorts (sorts:empty-sort-graph)]
                             [prior-imports (hash)])
                            ([import imports])
                    (values (sorts:merge-sort-graph
                             (operators:op-set-sorts
                              (module-ops (first import)))
                             prior-imports
                             sorts)
                            (merge-imports prior-imports (second import))))]
       [(sorts) (for/fold ([sorts sorts])
                          ([sort sort-list])
                  (sorts:add-sort sort hashcode all-imports sorts))]
       [(sorts) (for/fold ([sorts sorts])
                          ([sort-pair subsort-list])
                  (sorts:add-subsort (car sort-pair) (cdr sort-pair)
                                     hashcode all-imports sorts))]
       [(ops _) (for/fold ([ops (operators:empty-op-set sorts)]
                           [prior-imports (hash)])
                          ([import imports])
                  (values (operators:merge-op-set
                           (module-ops (first import))
                           prior-imports
                           ops)
                          (merge-imports prior-imports (second import))))]
       [(ops) (for/fold ([ops ops])
                        ([op-spec op-list])
                (match-let ([(list symbol domain range properties)
                             op-spec])
                  (operators:add-op symbol domain range
                                    properties hashcode all-imports ops)))]
       [(ops) (for/fold ([ops ops])
                        ([s-op-symbol s-op-list])
                (operators:add-special-op s-op-symbol hashcode all-imports ops))]
       [(rules) (rules:empty-rules)])
    (for/fold ([prior-imports (hash)])
              ([import imports])
      (rules:merge-rules! (module-rules (first import))
                          prior-imports
                          rules)
      (merge-imports prior-imports (second import)))
    (for ([fn-spec fn-list])
      (match-let ([(list symbol proc-expr) fn-spec])
        (rules:add-proc! symbol proc-expr hashcode rules)))
    (let ([mod (module module-name ops rules all-imports meta-terms hashcode)])
      (register-module mod)
      mod)))
