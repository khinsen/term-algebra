#lang racket

(provide meta truth equality string)

(require (prefix-in terms: term-algebra/terms)
         (prefix-in modules: term-algebra/modules))

(define meta modules:meta)

(modules:define-module truth
  (op true)
  (op false))

(modules:define-module equality
  (use truth)
  (op (== term1 term2)))

(let ([true (terms:term (modules:op-from equality 'true) '())]
      [false (terms:term (modules:op-from equality 'false) '())])
  (modules:make-special-rule equality '==
    (lambda (term1 term2)
      (if (equal? term1 term2)
          true
          false))))

(define string (modules:make-special-module 'string
                                            (hash) (set) (set 'string)))
