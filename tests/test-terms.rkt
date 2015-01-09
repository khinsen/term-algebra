#lang racket

(provide term-tests)

(require rackunit
         (only-in term-algebra/modules define-builtin-module
                                       sort-from module-sorts module-ops)
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in modules: term-algebra/modules)
         (prefix-in terms: term-algebra/terms))

(define-builtin-module test
  (sorts A B C X Y Z)
  (subsorts [A C] [B C] [X Y] [X Z])
  (op anA A)
  (op aB B)
  (op aC C)
  (op anX X)
  (op anY Y)
  (op (foo A) X)
  (op (foo C) Z)
  (op (bar A ...) Z)
  (op (bar X Y) Z))

(define anA (terms:make-term 'anA empty test))
(define aB (terms:make-term 'aB empty test))
(define aC (terms:make-term 'aC empty test))
(define anX (terms:make-term 'anX empty test))
(define anY (terms:make-term 'anY empty test))

(define-test-suite term-tests
  
  (test-case "nullary-terms"
    (check-equal? (terms:term-sort anA)
                  'A)
    (check-equal? (terms:term-sort aB)
                  'B)
    (check-equal? (terms:term-sort aC)
                  'C))
  
  (test-case "unary-terms"
    (check-equal? (terms:term-sort (terms:make-term 'foo (list anA) test))
                  'X)
    (check-equal? (terms:term-sort (terms:make-term 'foo (list aB) test))
                  'Z)
    (check-equal? (terms:term-sort (terms:make-term 'foo (list aC) test))
                  'Z))

  (test-case "binary-terms"
    (check-equal? (terms:term-sort (terms:make-term 'bar (list anX anY) test))
                  'Z)
    (check-equal? (terms:term-sort (terms:make-term 'bar (list anX anX) test))
                  'Z))

  (test-case "n-nary-terms"
    (check-equal? (terms:term-sort (terms:make-term 'bar (list anA) test))
                  'Z)
    (check-equal? (terms:term-sort (terms:make-term 'bar (list anA anA) test))
                  'Z)
    (check-equal? (terms:term-sort (terms:make-term 'bar (list anA anA anA)
                                                    test))
                  'Z))

  (test-exn "unknown-operator"
      #rx"Undefined operator.*"
    (lambda ()
      (terms:make-term 'baz (list anA) test)))

  (test-exn "wrong-number-of-args"
      #rx"Wrong number or sort of arguments.*"
    (lambda ()
      (terms:make-term 'foo empty test)))

  (test-exn "wrong-arg-sorts"
      #rx"Wrong number or sort of arguments.*"
    (lambda ()
      (terms:make-term 'foo (list anX) test))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests term-tests))
