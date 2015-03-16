#lang racket

(provide library-tests)

(require rackunit
         term-algebra/api
         term-algebra/library/boolean)

(define-syntax-rule (check-reduce module initial-term reduced-term)
  (check-equal? (reduce (term module initial-term))
                (term module reduced-term)))

(define-test-suite library-tests

  (test-case "boolean"
    (check-reduce boolean (not true)  false)
    (check-reduce boolean (not false) true)

    (check-reduce boolean (and true true)   true)
    (check-reduce boolean (and false false) false)
    (check-reduce boolean (and true false)  false)
    (check-reduce boolean (and false true)  false)
    (check-reduce boolean (and false true false true) false)
    (check-reduce boolean (and (not false) (not false)) true)

    (check-reduce boolean (or true true)   true)
    (check-reduce boolean (or false false) false)
    (check-reduce boolean (or true false)  true)
    (check-reduce boolean (or false true)  true)
    (check-reduce boolean (or false true false true) true)
    (check-reduce boolean (or (not false) (not false)) true)

    (check-reduce boolean (xor true true)   false)
    (check-reduce boolean (xor false false) false)
    (check-reduce boolean (xor true false)  true)
    (check-reduce boolean (xor false true)  true)
    (check-reduce boolean (xor false true false true) false)
    (check-reduce boolean (xor (not false) (not true)) true)))

(module* main #f
  (require rackunit/text-ui)
  (run-tests library-tests))
