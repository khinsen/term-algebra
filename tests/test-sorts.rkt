#lang racket

(provide sort-tests)

(require rackunit
         (only-in term-algebra/modules define-module term)
         (prefix-in builtin: term-algebra/builtin))

(define-test-suite sort-tests

  (test-exn "sort redefinition"
            #rx"sort already defined.*"
            (lambda () (define-module test
                    (sort a)
                    (sort b)
                    (sorts b c))
               test))

  (test-exn "missing sort definition for subsort"
            #rx"undefined sort.*"
            (lambda () (define-module test
                    (sort a)
                    (subsort a b))
               test))
  (test-exn "missing sort definition for subsort"
            #rx"undefined sort.*"
            (lambda () (define-module test
                    (sort b)
                    (subsort a b))
               test))
  
  (test-exn "equal-sorts-in-subsort"
            #rx"sorts are equal.*"
            (lambda () (define-module test
                    (sorts a b)
                    (subsort a a))
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
                    (sorts a b c)
                    (subsort a b)
                    (subsort b c)
                    (subsort c a))
               test))
  
  (test-exn "undefined-range-sort"
            #rx"undefined sort.*"
            (lambda () (define-module test
                    (op foo a))
               test))
  (test-exn "undefined-arg-sort"
            #rx"undefined sort.*"
            (lambda () (define-module test
                    (sort a)
                    (op (foo b) a))
               test))
  
  (test-exn "wrong-number-of-fixed-args"
            #rx"wrong number of arguments.*"
            (lambda () (define-module test
                    (use builtin:truth)
                    (op (foo Boolean) Boolean))
               (term test foo)))
  (test-exn "wrong-number-of-fixed-args"
            #rx"wrong number of arguments.*"
            (lambda () (define-module test
                    (use builtin:truth)
                    (op (foo Boolean) Boolean))
               (term test (foo true false))))
  (test-exn "wrong-number-of-variable-args"
            #rx"too few arguments.*"
            (lambda () (define-module test
                    (use builtin:truth)
                    (op (foo Boolean Boolean ...) Boolean))
               (term test (foo)))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests sort-tests))
