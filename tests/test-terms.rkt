#lang racket

(provide term-tests)

(require rackunit
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in signatures: term-algebra/signatures)
         (prefix-in terms: term-algebra/terms)
         (only-in term-algebra/modules
                   define-builtin-module module-signature))

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

(define test-sig (module-signature test))
(define anA (terms:make-term 'anA empty test-sig))
(define aB (terms:make-term 'aB empty test-sig))
(define aC (terms:make-term 'aC empty test-sig))
(define anX (terms:make-term 'anX empty test-sig))
(define anY (terms:make-term 'anY empty test-sig))

(define-test-suite term-tests
  
  (test-case "nullary-terms"
    (check-equal? (terms:term-sort anA)
                  'A)
    (check-equal? (terms:term-sort aB)
                  'B)
    (check-equal? (terms:term-sort aC)
                  'C))
  
  (test-case "unary-terms"
    (check-equal? (terms:term-sort (terms:make-term 'foo (list anA) test-sig))
                  'X)
    (check-equal? (terms:term-sort (terms:make-term 'foo (list aB) test-sig))
                  'Z)
    (check-equal? (terms:term-sort (terms:make-term 'foo (list aC) test-sig))
                  'Z))

  (test-case "binary-terms"
    (check-equal? (terms:term-sort 
                   (terms:make-term 'bar (list anX anY) test-sig))
                  'Z)
    (check-equal? (terms:term-sort 
                   (terms:make-term 'bar (list anX anX) test-sig))
                  'Z))

  (test-case "n-nary-terms"
    (check-equal? (terms:term-sort
                   (terms:make-term 'bar (list anA) test-sig))
                  'Z)
    (check-equal? (terms:term-sort
                   (terms:make-term 'bar (list anA anA) test-sig))
                  'Z)
    (check-equal? (terms:term-sort
                   (terms:make-term 'bar (list anA anA anA) test-sig))
                  'Z))

  (test-exn "unknown-operator"
      #rx"Undefined operator.*"
    (lambda ()
      (terms:make-term 'baz (list anA) test-sig)))

  (test-exn "wrong-number-of-args"
      #rx"Wrong number or sort of arguments.*"
    (lambda ()
      (terms:make-term 'foo empty test-sig)))

  (test-exn "wrong-arg-sorts"
      #rx"Wrong number or sort of arguments.*"
    (lambda ()
      (terms:make-term 'foo (list anX) test-sig))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests term-tests))
