#lang racket

(provide term-tests)

(require rackunit
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in terms: term-algebra/terms)
         (prefix-in builtin: term-algebra/builtin)
         (only-in term-algebra/modules
                   define-builtin-module module-ops))

(define-builtin-module test
  (use builtin:string)
  (use builtin:symbol)
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

(define test-ops (module-ops test))
(define anA (terms:make-term 'anA empty test-ops))
(define aB (terms:make-term 'aB empty test-ops))
(define aC (terms:make-term 'aC empty test-ops))
(define anX (terms:make-term 'anX empty test-ops))
(define anY (terms:make-term 'anY empty test-ops))
(define AVar (terms:var 'AVar 'A))
(define XVar (terms:var 'XVar 'X))

(define-test-suite term-tests
  
  (test-case "nullary-terms"
    (check-equal? (terms:term-sort anA)
                  'A)
    (check-equal? (terms:term-sort aB)
                  'B)
    (check-equal? (terms:term-sort aC)
                  'C))
  
  (test-case "unary-terms"
    (check-equal? (terms:term-sort (terms:make-term 'foo (list anA) test-ops))
                  'X)
    (check-equal? (terms:term-sort (terms:make-term 'foo (list aB) test-ops))
                  'Z)
    (check-equal? (terms:term-sort (terms:make-term 'foo (list aC) test-ops))
                  'Z))

  (test-case "binary-terms"
    (check-equal? (terms:term-sort 
                   (terms:make-term 'bar (list anX anY) test-ops))
                  'Z)
    (check-equal? (terms:term-sort 
                   (terms:make-term 'bar (list anX anX) test-ops))
                  'Z))

  (test-case "n-nary-terms"
    (check-equal? (terms:term-sort
                   (terms:make-term 'bar (list anA) test-ops))
                  'Z)
    (check-equal? (terms:term-sort
                   (terms:make-term 'bar (list anA anA) test-ops))
                  'Z)
    (check-equal? (terms:term-sort
                   (terms:make-term 'bar (list anA anA anA) test-ops))
                  'Z))

  (test-exn "unknown-operator"
      #rx"Undefined operator.*"
    (lambda ()
      (terms:make-term 'baz (list anA) test-ops)))

  (test-exn "wrong-number-of-args"
      #rx"Wrong number or sort of arguments.*"
    (lambda ()
      (terms:make-term 'foo empty test-ops)))
  (test-exn "wrong-number-of-args"
      #rx"Wrong number or sort of arguments.*"
    (lambda ()
      (terms:make-term 'foo (list anA anA) test-ops)))

  (test-exn "wrong-arg-sorts"
      #rx"Wrong number or sort of arguments.*"
    (lambda ()
      (terms:make-term 'foo (list anX) test-ops)))

  (test-case "pattern-matching"
    (check-equal? (terms:match-pattern 'foo 'foo test-ops)
                  (hash))
    (check-equal? (terms:match-pattern "foo" "foo" test-ops)
                  (hash))
    (check-equal? (terms:match-pattern 'foo "foo" test-ops)
                  #f)
    (check-equal? (terms:match-pattern AVar anA test-ops)
                  (hash AVar anA))
    (check-equal? (terms:match-pattern AVar 'foo test-ops)
                  #f)
    (check-equal? (terms:match-pattern
                   (terms:make-term 'foo (list anA) test-ops)
                   (terms:make-term 'foo (list anA) test-ops)
                   test-ops)
                  (hash))
    (check-equal? (terms:match-pattern
                   (terms:make-term 'foo (list AVar) test-ops)
                   (terms:make-term 'foo (list anA) test-ops)
                   test-ops)
                  (hash AVar anA))
    (check-equal? (terms:match-pattern
                   (terms:make-term 'foo (list AVar) test-ops)
                   anA
                   test-ops)
                  #f)
    (check-equal? (terms:match-pattern
                   (terms:make-term 'foo (list AVar) test-ops)
                   (terms:make-term 'foo (list aC) test-ops)
                   test-ops)
                  #f)
    (check-equal? (terms:match-pattern
                   (terms:make-term 'bar (list XVar anX) test-ops)
                   (terms:make-term 'bar (list anX  anX) test-ops)
                   test-ops)
                  (hash XVar anX))
    (check-equal? (terms:match-pattern
                   (terms:make-term 'bar (list XVar anX) test-ops)
                   (terms:make-term 'bar (list anX  anY) test-ops)
                   test-ops)
                  #f)
    (check-equal? (terms:match-pattern
                   (terms:make-term 'bar (list XVar anX) test-ops)
                   (terms:make-term 'bar
                                    (list (terms:make-term 'foo (list anA)
                                                           test-ops)
                                          anX)
                                    test-ops)
                   test-ops)
                  (hash XVar (terms:make-term 'foo (list anA) test-ops)))
    (check-equal? (terms:match-pattern
                   (terms:make-term 'bar (list XVar XVar) test-ops)
                   (terms:make-term 'bar
                                    (list (terms:make-term 'foo (list anA)
                                                           test-ops)
                                          anX)
                                    test-ops)
                   test-ops)
                  #f))
  
  (test-case "pattern-substitution"
    (check-equal?
     (terms:substitute (terms:make-term 'foo (list AVar) test-ops)
                       (hash AVar anA) test-ops)
     (terms:make-term 'foo (list anA) test-ops))
    (check-equal?
     (terms:substitute
      (terms:make-term 'bar (list (terms:make-term 'foo (list AVar) test-ops)
                                  anX)
                       test-ops)
      (hash AVar anA) test-ops)
     (terms:make-term 'bar (list (terms:make-term 'foo (list anA) test-ops)
                                  anX)
                      test-ops))
    (check-equal?
     (terms:substitute (terms:make-term 'bar (list XVar XVar) test-ops)
                       (hash XVar (terms:make-term 'foo (list anA) test-ops))
                       test-ops)
     (terms:make-term 'bar (list (terms:make-term 'foo (list anA) test-ops)
                                 (terms:make-term 'foo (list anA) test-ops))
                      test-ops))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests term-tests))
