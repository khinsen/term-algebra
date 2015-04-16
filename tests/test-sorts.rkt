#lang racket

(provide sort-tests)

(require rackunit
         (only-in term-algebra/nodes define-builtin-node node-ops)
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators))

(define (node-sorts node)
  (operators:op-set-sorts (node-ops node)))

(define-builtin-node sort-test-1
  (sorts A B C X Y Z)
  (subsorts [A C] [B C] [X Y] [Y Z]))

(define-builtin-node sort-test-2
  (include sort-test-1)
  (subsorts [A X]))


(define-test-suite sort-tests

  (test-exn "sort redefinition"
      #rx"sort already defined.*"
    (lambda () (define-builtin-node test-sorts-1
                 (sorts a b b c))
            (void)))

  (test-exn "missing sort definition for subsort"
      #rx"undefined sort.*"
    (lambda () (define-builtin-node test-sorts-2
                 (sorts a)
                 (subsorts [a b]))
            (void)))
  (test-exn "missing sort definition for subsort"
      #rx"undefined sort.*"
    (lambda () (define-builtin-node test-sorts-3
                 (sorts b)
                 (subsorts [a b]))
            (void)))

  (test-exn "equal-sorts-in-subsort"
      #rx"sorts are equal.*"
    (lambda () (define-builtin-node test-sorts-4
                 (sorts a b)
                 (subsorts [a a]))
            (void)))

  (test-exn "any-in-subsort"
      #rx"special sort Any forbidden in subsort relations"
    (lambda () (define-builtin-node test-sorts-5
                 (sorts a)
                 (subsorts [a Any]))
            (void)))
  (test-exn "any-in-subsort"
      #rx"special sort Any forbidden in subsort relations"
    (lambda () (define-builtin-node test-sorts-6
                 (sorts a)
                 (subsorts [Any a]))
            (void)))

  (test-exn "double subsort definition"
      #rx"subsort relation already defined.*"
    (lambda () (define-builtin-node test-sorts-7
                 (sorts a b)
                 (subsorts [a b] [a b]))
            (void)))

  (test-exn "cyclic subsort definition"
      #rx"cyclic subsort dependence.*"
    (lambda () (define-builtin-node test-sorts-8
                 (sorts a b c)
                 (subsorts [a b] [b c] [c a]))
            (void)))

  (test-exn "sort redefinition after import"
      #rx"sort already defined.*"
    (lambda () (define-builtin-node test-sorts-9
                 (use sort-test-1)
                 (sorts A B))
            (void)))

  (test-not-exn "direct and indirect import"
    (lambda () (define-builtin-node test-sorts-10
                 (use sort-test-1)
                 (use sort-test-2))
            (void)))
  (test-exn "restricted import"
      #rx"both sorts from restricted import.*"
    (lambda () (define-builtin-node test-sorts-11
                 (use sort-test-1)
                 (subsorts [A X]))
            (void)))
  (test-not-exn "unrestricted import"
    (lambda () (define-builtin-node test-sorts-12
                 (include sort-test-1)
                 (subsorts [A X]))
            (void)))
  (test-not-exn "restricted and unrestricted import"
    (lambda () (define-builtin-node test-sorts-13
                 (use sort-test-1))
               (define-builtin-node test-sorts-14
                 (use test-sorts-13)
                 (include sort-test-1)
                 (subsorts [A X]))
            (void)))

  (test-case "kinds-1"
    (check-equal? (sorts:kind 'A (node-sorts sort-test-1))
                  (set 'A 'B 'C))
    (check-equal? (sorts:kind 'B (node-sorts sort-test-1))
                  (set 'A 'B 'C))
    (check-equal? (sorts:kind 'C (node-sorts sort-test-1))
                  (set 'A 'B 'C))
    (check-equal? (sorts:kind 'X (node-sorts sort-test-1))
                  (set 'X 'Y 'Z))
    (check-equal? (sorts:kind 'Y (node-sorts sort-test-1))
                  (set 'X 'Y 'Z))
    (check-equal? (sorts:kind 'Z (node-sorts sort-test-1))
                  (set 'X 'Y 'Z)))
  (test-case "kinds-2"
    (check-equal? (sorts:kind 'A (node-sorts sort-test-2))
                  (set 'A 'B 'C 'X 'Y 'Z))
    (check-equal? (sorts:kind 'B (node-sorts sort-test-2))
                  (set 'A 'B 'C 'X 'Y 'Z))
    (check-equal? (sorts:kind 'C (node-sorts sort-test-2))
                  (set 'A 'B 'C 'X 'Y 'Z))
    (check-equal? (sorts:kind 'X (node-sorts sort-test-2))
                  (set 'A 'B 'C 'X 'Y 'Z))
    (check-equal? (sorts:kind 'Y (node-sorts sort-test-2))
                  (set 'A 'B 'C 'X 'Y 'Z))
    (check-equal? (sorts:kind 'Z (node-sorts sort-test-2))
                  (set 'A 'B 'C 'X 'Y 'Z)))

  (test-case "subsorts"
    (check-equal? (sorts:subsorts 'A (node-sorts sort-test-1))
                  (set 'A))
    (check-equal? (sorts:subsorts 'B (node-sorts sort-test-1))
                  (set 'B))
    (check-equal? (sorts:subsorts 'C (node-sorts sort-test-1))
                  (set 'A 'B 'C))
    (check-equal? (sorts:subsorts 'X (node-sorts sort-test-1))
                  (set 'X))
    (check-equal? (sorts:subsorts 'Y (node-sorts sort-test-1))
                  (set 'X 'Y))
    (check-equal? (sorts:subsorts 'Z (node-sorts sort-test-1))
                  (set 'X 'Y 'Z))
    (check-equal? (sorts:subsorts 'X (node-sorts sort-test-2))
                  (set 'A 'X))
    (check-equal? (sorts:subsorts 'Y (node-sorts sort-test-2))
                  (set 'A 'X 'Y))
    (check-equal? (sorts:subsorts 'Z (node-sorts sort-test-2))
                  (set 'A 'X 'Y 'Z))
    (check-equal? (sorts:least-common-sort (node-sorts sort-test-1) 'A 'B)
                  'C)
    (check-equal? (sorts:least-common-sort (node-sorts sort-test-1) 'A 'C)
                  'C)
    (check-equal? (sorts:least-common-sort (node-sorts sort-test-1) 'A 'B 'C)
                  'C)
    (check-equal? (sorts:least-common-sort (node-sorts sort-test-2) 'A 'Y)
                  'Y)))


(module* main #f
  (require rackunit/text-ui)
  (run-tests sort-tests))
