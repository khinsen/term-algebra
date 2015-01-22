#lang racket

(provide rewrite-tests)

(require rackunit
         (only-in term-algebra/rewrite reduce)
         (only-in term-algebra/syntax define-module term)
         (prefix-in builtin: term-algebra/builtin))

(define-module boolean

  (use builtin:truth)
 
  (op (not Boolean) Boolean)
  (=-> (not true) false)
  (=-> (not false) true)

  (op (and Boolean Boolean) Boolean)
  (=-> (and true true) true)
  (=-> #:var [X Boolean] (and false X) false)
  (=-> #:var [X Boolean] (and X false) false))

(define-module test
  (use builtin:equality)
  (sort A)
  (op (foo A A) A)
  (op bar A)
  (op baz A)
  (=-> #:vars ([A1 A] [A2 A])
       (foo A1 A2) #:if (== A1 A2) A1))

(define-test-suite rewrite-tests

  (test-case "boolean"
    (check-equal? (reduce (term boolean (not true)) boolean)
                  (term boolean false))
    (check-equal? (reduce (term boolean (not false)) boolean)
                  (term boolean true))
    (check-equal? (reduce (term boolean (and true true)) boolean)
                  (term boolean true))
    (check-equal? (reduce (term boolean (and true false)) boolean)
                  (term boolean false))
    (check-equal? (reduce (term boolean (and false true)) boolean)
                  (term boolean false))
    (check-equal? (reduce (term boolean (and false false)) boolean)
                  (term boolean false))
    (check-equal? (reduce (term boolean (and false (not false))) boolean)
                  (term boolean false))
    (check-equal? (reduce (term boolean (and (not false) (not false))) boolean)
                  (term boolean true)))
  
  (test-case "test"
    (check-equal? (reduce (term test (foo bar bar)) test)
                  (term test bar))
    (check-equal? (reduce (term test (foo bar (foo bar bar))) test)
                  (term test bar))
    (check-equal? (reduce (term test (foo bar baz)) test)
                  (term test (foo bar baz)))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests rewrite-tests))
