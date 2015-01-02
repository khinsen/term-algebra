#lang racket

(provide (struct-out op)
         (struct-out var)
         (struct-out term)
         vars-in-term
         term-sort)

; Struct definitions

(struct op (symbol domain range properties [rules #:mutable])
        #:transparent
        #:property prop:custom-write
        (lambda (op port mode)
          (if (null? (op-domain op))
              (write (list 'op (op-symbol op) (op-range op)) port)
              (write (list 'op (op-symbol op) 
                           (cons (op-symbol op) (op-domain op))
                           (op-range op))
                     port))))

(struct var (symbol sort)
        #:transparent
        #:property prop:custom-write
        (lambda (var port mode)
          (write (list (var-symbol var) (var-sort var)) port)))

(struct term (op args)
        #:transparent
        #:property prop:custom-write
        (lambda (term port mode)
          (let ([op (term-op term)])
            (if (null? (op-domain op))
                (write (op-symbol op) port)
                (write (cons (op-symbol op) (term-args term)) port)))))

; Basic operations

(define (vars-in-term term)
  (cond
   [(var? term)  (seteq term)]
   [(term? term) (let ([args (term-args term)])
                   (if (empty? args)
                       (seteq)
                       (apply set-union (map vars-in-term args))))]
   [else         (seteq)]))

(define (term-sort gterm)
  (cond
   [(term? gterm)   (op-range (term-op gterm))]
   [(var? gterm)    (var-sort gterm)]
   [(symbol? gterm) 'Symbol]
   [(string? gterm) 'String]
   [(number? gterm) 'Rational]
   [else (error "unknown term type" gterm)]))
