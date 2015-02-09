#lang racket

(provide (struct-out module)
         (struct-out vterm)
         define-builtin-module
         sort-from op-from
         lookup-module-hash
         make-module
         make-term make-vterm)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in terms: term-algebra/terms)
         (for-syntax syntax/parse)
         (only-in file/sha1 sha1))

;
; The data structure for the internal module representation
;
(struct module (name ops rules meta hashcode)
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
(define (hash-of-string s)
  (sha1 (open-input-string s)))

(define (hash-of-meta-module meta-module)
  (let ([o (open-output-string)])
    (write meta-module o)
    (hash-of-string (get-output-string o))))

(define *modules* (make-hash))

(define (register-module module)
  (hash-set! *modules* (module-hashcode module) (make-weak-box module)))

(define (lookup-module-hash hash)
  (weak-box-value (hash-ref *modules* hash)))

(define (make-module module-name ops rules meta-terms)
  (let* ([hashcode (if meta-terms
                       (hash-of-meta-module meta-terms)
                       (hash-of-string (symbol->string module-name)))]
         [mod (module module-name ops rules meta-terms hashcode)])
    (register-module mod)
    mod))

(define (sort-from module sort-symbol)
  (if (sorts:has-sort? sort-symbol
                       (operators:op-set-sorts (module-ops module)))
      sort-symbol
      (error (format "no sort ~s in module ~s" sort-symbol module))))

(define (op-from module op-symbol)
  (if (operators:has-op? op-symbol
                         (module-ops module))
      op-symbol
      (error (format "no op ~s in module ~s" op-symbol module))))

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
    (pattern ((~datum extend) module:id)
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
         ((~datum sorts) sort:id ...))
        (~optional
         ((~datum subsorts) (sort1:id sort2:id) ...))
        (~optional
         ((~datum special-ops) s-op:id ...))
        op-decl:op-builtin ...)
     (with-syntax ([import-list (if (attribute import-decl.module)
                                    #'(map (lambda (a b) (cons a b))
                                           (list import-decl.module ...)
                                           (list import-decl.use ...))
                                    #'(list))]
                   [sort-list (if (attribute sort)
                                  #'(list (quote sort) ...)
                                  #'(list))]
                   [subsort-list (if (attribute sort1)
                                     #'(list (cons (quote sort1)
                                                   (quote sort2))
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
           (let* ([sorts (for/fold ([sorts (sorts:empty-sort-graph)])
                                   ([import import-list])
                           (sorts:merge-sort-graph
                            (operators:op-set-sorts (module-ops (car import)))
                            sorts))]
                  [sorts (foldl sorts:add-sort sorts sort-list)]
                  [sorts (for/fold ([sorts sorts])
                                   ([sort-pair subsort-list])
                           (sorts:add-subsort (car sort-pair) (cdr sort-pair)
                                              sorts))]
                  [ops (for/fold ([ops (operators:empty-op-set sorts)])
                                 ([mod import-list])
                         (operators:merge-op-set (module-ops (car mod))
                                                 (cdr mod) ops))]
                  [ops (for/fold ([ops ops])
                                 ([op-spec op-list])
                         (match-let ([(list symbol domain range properties)
                                      op-spec])
                           (operators:add-op symbol domain range
                                             properties ops)))]
                  [ops (for/fold ([ops ops])
                                 ([s-op-symbol s-op-list])
                         (operators:add-special-op s-op-symbol ops))]
                  [rules (for*/fold ([rules (hash)])
                                    ([import import-list]
                                     [kv (hash->list
                                          (module-rules (car import)))])
                           (hash-update rules (first kv)
                                        (λ (l) (append l (cdr kv))) empty))]
                  [rules (for/fold ([rules rules])
                                   ([fn-spec fn-list])
                           (match-let ([(list symbol proc-expr) fn-spec])
                             (hash-update rules symbol
                                          (λ (l) (append l (list proc-expr)))
                                          empty)))])
             (make-module (quote module-name) ops rules #f))))]))
