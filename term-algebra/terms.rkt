#lang racket

(provide (struct-out op)
         (struct-out var)
         (struct-out term)
         (struct-out sort)
         vars-in-term)

; Struct definitions

(struct op (symbol domain range [rules #:mutable])
        #:transparent
        #:property prop:custom-write
        (lambda (op port mode)
          (if (null? (op-domain op))
              (write (list 'op (op-symbol op) (op-range op)) port)
              (write (list 'op (op-symbol op) 
                           (cons (op-symbol op) (op-domain op))
                           (op-range op))
                     port))))

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
            (if (null? (op-domain op))
                (write (op-symbol op) port)
                (write (cons (op-symbol op) (term-args term)) port)))))

(struct sort (symbol)
        #:transparent
        #:property prop:custom-write
        (lambda (sort port mode)
          (write (sort-symbol sort) port)))

; Basic operations

(define (vars-in-term term)
  (cond
   [(var? term)  (seteq term)]
   [(term? term) (let ([args (term-args term)])
                   (if (empty? args)
                       (seteq)
                       (apply set-union (map vars-in-term args))))]
   [else         (seteq)]))
