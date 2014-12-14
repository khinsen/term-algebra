#lang racket

(provide truth equality meta)

(require (prefix-in terms: term-algebra/terms)
         (prefix-in modules: term-algebra/modules))

(modules:define-module truth
  (define-op true)
  (define-op false))

(modules:define-module equality
  (use truth)
  (define-op (== term1 term2)))

(let ([true (terms:term (terms:module-op equality 'true) '())]
      [false (terms:term (terms:module-op equality 'false) '())])
  (terms:make-special-op equality '==
    (lambda (term1 term2)
      (if (equal? term1 term2)
          true
          false))))

(define meta modules:meta)
