#lang racket

(provide extended-syntax-tests)

(require rackunit
         term-algebra/api
         (rename-in term-algebra/library/list [list list-module]))

(define-syntax-rule (check-reduce module initial-term reduced-term)
  (check-equal? (reduce (term module initial-term))
                (term module reduced-term)))

(define-module test
        (include list-module
                 #:transforms (add-import (use builtin:string))
                 (rename-sort Element String)
                 (rename-sort List StringList)
                 (rename-sort NonEmptyList NEStringList)
                 (rename-op list string-list))
        (op bar NEStringList)
        (=-> bar (string-list "a" "b" "c")))

(define-test-suite extended-syntax-tests

  (check-reduce test (tail bar) (string-list "b" "c")))

(module* main #f
  (require rackunit/text-ui)
  (run-tests extended-syntax-tests))
