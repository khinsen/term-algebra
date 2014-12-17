#lang racket

(provide sort-tests)

(require rackunit
         (only-in term-algebra/modules define-module))

(define-test-suite sort-tests

  (test-exn "sort redefinition"
            #rx"sort already defined.*"
            (lambda () (define-module test
                    (sort a)
                    (sort b)
                    (sorts b c))
               test))

  (test-exn "missing sort definition"
            #rx"sort undefined.*"
            (lambda () (define-module test
                    (sort a)
                    (subsort a b))
               test))
  
  (test-exn "double subsort definition"
            #rx"subsort relation already defined.*"
            (lambda () (define-module test
                    (sorts a b)
                    (subsort a b)
                    (subsort a b))
               test))

  (test-exn "cyclic subsort definition"
            #rx"cyclic subsort dependence.*"
            (lambda () (define-module test
                    (sorts a b)
                    (subsort a b)
                    (subsort b a))
               test)))

(module* main #f
  (require rackunit/text-ui)
  (run-tests sort-tests))
