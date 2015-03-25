#lang racket/base

(require rackunit
         "test-sorts.rkt"
         "test-operators.rkt"
         "test-terms.rkt"
         "test-meta.rkt"
         "test-rewrite.rkt"
         "test-builtin.rkt"
         "test-modules.rkt"
         "test-library.rkt"
         "test-peano-numbers.rkt"
         "test-extended-syntax.rkt"
         )

(provide all-tests)

(define all-tests
  (make-test-suite "All term algebra tests"
    (list sort-tests
          operator-tests
          term-tests
          meta-tests
          rewrite-tests
          builtin-tests
          module-tests
          library-tests
          peano-number-tests
          extended-syntax-tests
          )))

(module* main #f
  (require rackunit/text-ui)
  (run-tests all-tests))
