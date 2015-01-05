#lang racket

(provide sort-tests)

(require rackunit
         (only-in term-algebra/syntax define-module term)
         (only-in term-algebra/modules sort-from module-sorts)
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in builtin: term-algebra/builtin))

(define-module sort-test-1
  (sorts A B C X Y Z)
  (subsorts [A C] [B C] [X Y] [Y Z]))

(define-module sort-test-2
  (extend sort-test-1)
  (subsort A X))

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

  (test-exn "any-in-subsort"
            #rx"special sort Any forbidden in subsort relations"
            (lambda () (define-module test
                    (sort a)
                    (subsort a Any))
               test))
  (test-exn "any-in-subsort"
            #rx"special sort Any forbidden in subsort relations"
            (lambda () (define-module test
                    (sort a)
                    (subsort Any a))
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

  (test-case "kinds-1"
    (check-equal? (sorts:kind 'A (module-sorts sort-test-1))
                  (set 'A 'B 'C))
    (check-equal? (sorts:kind 'B (module-sorts sort-test-1))
                  (set 'A 'B 'C))
    (check-equal? (sorts:kind 'C (module-sorts sort-test-1))
                  (set 'A 'B 'C))
    (check-equal? (sorts:kind 'X (module-sorts sort-test-1))
                  (set 'X 'Y 'Z))
    (check-equal? (sorts:kind 'Y (module-sorts sort-test-1))
                  (set 'X 'Y 'Z))
    (check-equal? (sorts:kind 'Z (module-sorts sort-test-1))
                  (set 'X 'Y 'Z)))
  (test-case "kinds-2"
    (check-equal? (sorts:kind 'A (module-sorts sort-test-2))
                  (set 'A 'B 'C 'X 'Y 'Z))
    (check-equal? (sorts:kind 'B (module-sorts sort-test-2))
                  (set 'A 'B 'C 'X 'Y 'Z))
    (check-equal? (sorts:kind 'C (module-sorts sort-test-2))
                  (set 'A 'B 'C 'X 'Y 'Z))
    (check-equal? (sorts:kind 'X (module-sorts sort-test-2))
                  (set 'A 'B 'C 'X 'Y 'Z))
    (check-equal? (sorts:kind 'Y (module-sorts sort-test-2))
                  (set 'A 'B 'C 'X 'Y 'Z))
    (check-equal? (sorts:kind 'Z (module-sorts sort-test-2))
                  (set 'A 'B 'C 'X 'Y 'Z)))
  
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
