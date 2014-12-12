#lang racket

(provide truth meta)

(require (prefix-in terms: term-algebra/terms)
         (prefix-in modules: term-algebra/modules))

(modules:define-module truth
  (define-op true)
  (define-op false)
  (define-op (== term1 term2)))

(let ([true (terms:term (terms:module-op truth 'true) '())]
      [false (terms:term (terms:module-op truth 'false) '())])
  (terms:make-special-op truth '==
    (lambda (term1 term2)
      (if (equal? term1 term2)
          true
          false))))

(define meta modules:meta)
