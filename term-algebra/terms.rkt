#lang racket

(provide (struct-out op)
         (struct-out var)
         (struct-out term)
         (struct-out module)
         module-op make-special-op)

; Struct definitions

(struct op (symbol args [rules #:mutable])
        #:transparent
        #:property prop:custom-write
        (lambda (op port mode)
          (if (null? (op-args op))
              (write (op-symbol op) port)
              (write (cons (op-symbol op) (op-args op)) port))))

(struct var (symbol)
        #:transparent
        #:property prop:custom-write
        (lambda (var port mode)
          (write (var-symbol var) port)))

(struct term (op args)
        #:transparent
        #:property prop:custom-write
        (lambda (term port mode)
          (let ([op (term-op term)])
            (if (null? (op-args op))
                (write (op-symbol op) port)
                (write (cons (op-symbol op) (term-args term)) port)))))

(struct module (name ops meta)
        #:transparent)

; Basic operations

(define (module-op module op-symbol)
  (hash-ref (module-ops module) op-symbol))

(define (make-special-op module op-symbol proc)
  (let ([op (hash-ref (module-ops module) op-symbol)])
    (if (empty? (op-rules op))
        (set-op-rules! op proc)
        (error "non-empty rule list for operator " op))))
