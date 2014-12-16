#lang racket

(provide truth equality meta)

(require (prefix-in terms: term-algebra/terms)
         (prefix-in modules: term-algebra/modules))

(modules:define-module truth
  (op true)
  (op false))

(modules:define-module equality
  (use truth)
  (op (== term1 term2)))

(let ([true (terms:term (modules:op-from equality 'true) '())]
      [false (terms:term (modules:op-from equality 'false) '())])
  (modules:make-special-op equality '==
    (lambda (term1 term2)
      (if (equal? term1 term2)
          true
          false))))

(define meta modules:meta)
