#lang racket

(require rackunit
         (only-in term-algebra/rewrite reduce)
         (only-in term-algebra/modules term)
         (only-in term-algebra/builtin equality))

(define-syntax-rule (check-reduce module initial-term reduced-term)
  (check-equal? (reduce (term module initial-term)) (term module reduced-term)))

(test-case "equality"

  (check-reduce equality (== false false)
                         true)
  (check-reduce equality (== true true)
                         true)
  (check-reduce equality (== false true)
                         false)
  (check-reduce equality (== true false)
                         false)
  (check-reduce equality (== (== true true) (== false false))
                         true))
