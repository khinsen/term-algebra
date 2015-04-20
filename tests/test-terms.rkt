#lang racket

(provide term-tests)

(require rackunit
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in terms: term-algebra/terms)
         (prefix-in builtin: term-algebra/builtin)
         (only-in term-algebra/nodes
                   define-builtin-node node-ops))

(define-builtin-node test-terms
  (use builtin:string)
  (use builtin:symbol)
  (sorts A B C X Y Z)
  (subsorts [A C] [B C] [X Y] [X Z])
  (op anA A)
  (op anotherA A)
  (op aB B)
  (op aC C)
  (op anX X)
  (op anY Y)
  (op (foo A) X)
  (op (foo C) Z)
  (op (bar A ...) Z)
  (op (bar X Y) Z)
  (op (sym A ...) A #:symmetric))

(define test-ops (node-ops test-terms))
(define anA (terms:make-term 'anA empty test-ops))
(define anotherA (terms:make-term 'anotherA empty test-ops))
(define aB (terms:make-term 'aB empty test-ops))
(define aC (terms:make-term 'aC empty test-ops))
(define anX (terms:make-term 'anX empty test-ops))
(define anY (terms:make-term 'anY empty test-ops))
(define AVar (terms:var 'AVar 'A))
(define XVar (terms:var 'XVar 'X))

(define-test-suite term-tests
  
  (test-case "nullary-terms"
    (check-equal? (terms:sort-of anA)
                  'A)
    (check-equal? (terms:sort-of aB)
                  'B)
    (check-equal? (terms:sort-of aC)
                  'C))
  
  (test-case "unary-terms"
    (check-equal? (terms:sort-of (terms:make-term 'foo (list anA) test-ops))
                  'X)
    (check-equal? (terms:sort-of (terms:make-term 'foo (list aB) test-ops))
                  'Z)
    (check-equal? (terms:sort-of (terms:make-term 'foo (list aC) test-ops))
                  'Z))

  (test-case "binary-terms"
    (check-equal? (terms:sort-of 
                   (terms:make-term 'bar (list anX anY) test-ops))
                  'Z)
    (check-equal? (terms:sort-of 
                   (terms:make-term 'bar (list anX anX) test-ops))
                  'Z))

  (test-case "n-nary-terms"
    (check-equal? (terms:sort-of
                   (terms:make-term 'bar (list anA) test-ops))
                  'Z)
    (check-equal? (terms:sort-of
                   (terms:make-term 'bar (list anA anA) test-ops))
                  'Z)
    (check-equal? (terms:sort-of
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
                  #f)
    (check-equal? (terms:match-pattern
                   (terms:make-term 'sym (list AVar anA) test-ops)
                   (terms:make-term 'sym (list anA  anA) test-ops)
                   test-ops)
                  (hash AVar anA))
    (check-equal? (terms:match-pattern
                   (terms:make-term 'sym (list anA) test-ops)
                   (terms:make-term 'sym (list anotherA) test-ops)
                   test-ops)
                  #f)
    (check-equal? (terms:match-pattern
                   (terms:make-term 'sym (list anA anotherA) test-ops)
                   (terms:make-term 'sym (list anotherA anA) test-ops)
                   test-ops)
                  (hash))
    (check-equal? (terms:match-pattern
                   (terms:make-term 'sym (list anA AVar) test-ops)
                   (terms:make-term 'sym (list anotherA anA) test-ops)
                   test-ops)
                  (hash AVar anotherA)))
  
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
