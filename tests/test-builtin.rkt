#lang racket

(require rackunit
         (only-in term-algebra/rewrite reduce)
         (only-in term-algebra/modules term)
         (only-in term-algebra/builtin truth))

(define-syntax-rule (check-reduce module initial-term reduced-term)
  (check-equal? (reduce (term module initial-term)) (term module reduced-term)))

(test-case "truth"

  (check-reduce truth (== false false)
                      true)
  (check-reduce truth (== true true)
                      true)
  (check-reduce truth (== false true)
                      false)
  (check-reduce truth (== true false)
                      false)
  (check-reduce truth (== (== true true) (== false false))
                      true))
