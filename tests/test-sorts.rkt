#lang racket

(provide sort-tests)

(require rackunit
         (only-in term-algebra/modules define-builtin-module module-ops)
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators))

(define (module-sorts module)
  (operators:op-set-sorts (module-ops module)))

(define-builtin-module sort-test-1
  (sorts A B C X Y Z)
  (subsorts [A C] [B C] [X Y] [Y Z]))

(define-builtin-module sort-test-2
  (include sort-test-1)
  (subsorts [A X]))


(define-test-suite sort-tests

  (test-exn "sort redefinition"
      #rx"sort already defined.*"
    (lambda () (define-builtin-module test
                 (sorts a b b c))
            test))

  (test-exn "missing sort definition for subsort"
      #rx"undefined sort.*"
    (lambda () (define-builtin-module test
                 (sorts a)
                 (subsorts [a b]))
            test))
  (test-exn "missing sort definition for subsort"
      #rx"undefined sort.*"
    (lambda () (define-builtin-module test
                 (sorts b)
                 (subsorts [a b]))
            test))

  (test-exn "equal-sorts-in-subsort"
      #rx"sorts are equal.*"
    (lambda () (define-builtin-module test
                 (sorts a b)
                 (subsorts [a a]))
            test))

  (test-exn "any-in-subsort"
      #rx"special sort Any forbidden in subsort relations"
    (lambda () (define-builtin-module test
                 (sorts a)
                 (subsorts [a Any]))
            test))
  (test-exn "any-in-subsort"
      #rx"special sort Any forbidden in subsort relations"
    (lambda () (define-builtin-module test
                 (sorts a)
                 (subsorts [Any a]))
            test))

  (test-exn "double subsort definition"
      #rx"subsort relation already defined.*"
    (lambda () (define-builtin-module test
                 (sorts a b)
                 (subsorts [a b] [a b]))
            test))

  (test-exn "cyclic subsort definition"
      #rx"cyclic subsort dependence.*"
    (lambda () (define-builtin-module test
                 (sorts a b c)
                 (subsorts [a b] [b c] [c a]))
            test))

  (test-exn "sort redefinition after import"
      #rx"sort already defined.*"
    (lambda () (define-builtin-module test
                 (use sort-test-1)
                 (sorts A B))
            test))

  (test-not-exn "direct and indirect import"
    (lambda () (define-builtin-module import-test
                 (use sort-test-1)
                 (use sort-test-2))
            import-test))
  (test-exn "restricted import"
      #rx"both sorts from restricted import.*"
    (lambda () (define-builtin-module import-test
                 (use sort-test-1)
                 (subsorts [A X]))
            import-test))
  (test-not-exn "unrestricted import"
    (lambda () (define-builtin-module import-test
                 (include sort-test-1)
                 (subsorts [A X]))
            import-test))
  (test-not-exn "restricted and unrestricted import"
    (lambda () (define-builtin-module import-test-1
                 (use sort-test-1))
               (define-builtin-module import-test-2
                 (use import-test-1)
                 (include sort-test-1)
                 (subsorts [A X]))
            import-test-2))

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

  (test-case "subsorts"
    (check-equal? (sorts:subsorts 'A (module-sorts sort-test-1))
                  (set 'A))
    (check-equal? (sorts:subsorts 'B (module-sorts sort-test-1))
                  (set 'B))
    (check-equal? (sorts:subsorts 'C (module-sorts sort-test-1))
                  (set 'A 'B 'C))
    (check-equal? (sorts:subsorts 'X (module-sorts sort-test-1))
                  (set 'X))
    (check-equal? (sorts:subsorts 'Y (module-sorts sort-test-1))
                  (set 'X 'Y))
    (check-equal? (sorts:subsorts 'Z (module-sorts sort-test-1))
                  (set 'X 'Y 'Z))
    (check-equal? (sorts:subsorts 'X (module-sorts sort-test-2))
                  (set 'A 'X))
    (check-equal? (sorts:subsorts 'Y (module-sorts sort-test-2))
                  (set 'A 'X 'Y))
    (check-equal? (sorts:subsorts 'Z (module-sorts sort-test-2))
                  (set 'A 'X 'Y 'Z))))


(module* main #f
  (require rackunit/text-ui)
  (run-tests sort-tests))
