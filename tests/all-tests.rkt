#lang racket/base

(require rackunit
         "test-builtin.rkt"
         "test-boolean.rkt"
         "test-peano-numbers.rkt")

(provide all-tests)

(define all-tests
  (make-test-suite "All term algebra tests"
    (list builtin-tests
          boolean-tests
          peano-number-tests)))

(module* main #f
  (require rackunit/text-ui)
  (run-tests all-tests))
