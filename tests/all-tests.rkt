#lang racket/base

(require rackunit
         "test-sorts.rkt"
         "test-operators.rkt"
         ;; "test-modules.rkt"
         "test-terms.rkt"
         ;; "test-builtin.rkt"
         ;; "test-boolean.rkt"
         ;; "test-peano-numbers.rkt"
         )

(provide all-tests)

(define all-tests
  (make-test-suite "All term algebra tests"
    (list sort-tests
          operator-tests
          ;; module-tests
          term-tests
          ;; builtin-tests
          ;; boolean-tests
          ;; peano-number-tests
          )))

(module* main #f
  (require rackunit/text-ui)
  (run-tests all-tests))
