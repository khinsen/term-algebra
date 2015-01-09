#lang racket

(provide (struct-out term)
         (struct-out var)
         vars-in-term
         sort-of
         make-term)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in signatures: term-algebra/signatures))

; Struct definitions

(struct term (op args sort signature)
        #:transparent
        #:property prop:custom-write
        (lambda (term port mode)
          (let ([op (term-op term)])
            (if (null? (term-args term))
                (write op port)
                (write (cons op (term-args term)) port)))))

(struct var (symbol sort sigature)
        #:transparent
        #:property prop:custom-write
        (lambda (var port mode)
          (write (list (var-symbol var) (var-sort var)) port)))

; Basic operations

(define (vars-in-term term)
  (cond
   [(var? term)  (seteq term)]
   [(term? term) (let ([args (term-args term)])
                   (if (empty? args)
                       (seteq)
                       (apply set-union (map vars-in-term args))))]
   [else         (seteq)]))

(define (sort-of gterm)
  (cond
   [(term? gterm)   (term-sort gterm)]
   [(var? gterm)    (var-sort gterm)]
   [(symbol? gterm) 'Symbol]
   [(string? gterm) 'String]
   [(number? gterm) 'Rational]
   [else (error "unknown term type" gterm)]))

(define (make-term op args signature)
  (unless (andmap (λ (t) (equal? (term-signature t) signature)) args)
    (error "Argument terms defined in a different module"))
  (unless (operators:has-op? op (signatures:signature-ops signature))
    (error "Undefined operator " op))
  (let ([sort
         (operators:lookup-op op
                              (map sort-of args)
                              (signatures:signature-ops signature))])
    (unless sort
      (error "Wrong number or sort of arguments: " (cons op args)))
    (term op args sort signature)))
