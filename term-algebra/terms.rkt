#lang racket

(provide (struct-out op)
         (struct-out var)
         (struct-out term))

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
