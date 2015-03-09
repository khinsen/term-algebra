#lang racket

(provide rewrite-tests)

(require rackunit
         term-algebra/api)

(define-module boolean

  (use builtin:truth)

  (op (not Boolean) Boolean)
  (=-> (not true) false)
  (=-> (not false) true)

  (op (and Boolean Boolean) Boolean)
  (=-> (and true true) true)
  (=-> #:var [X Boolean] (and false X) false)
  (=-> #:var [X Boolean] (and X false) false)

  (op (or Boolean ...) Boolean)
  (=-> #:var [X Boolean] (or X) X)
  (=-> #:var [Xs Boolean ...] (or true Xs) true)
  (=-> #:var [Xs Boolean ...] (or false  Xs) (or Xs)))

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
    (check-equal? (reduce (term boolean (not true)))
                  (term boolean false))
    (check-equal? (reduce (term boolean (not false)))
                  (term boolean true))
    (check-equal? (reduce (term boolean (and true true)))
                  (term boolean true))
    (check-equal? (reduce (term boolean (and true false)))
                  (term boolean false))
    (check-equal? (reduce (term boolean (and false true)))
                  (term boolean false))
    (check-equal? (reduce (term boolean (and false false)))
                  (term boolean false))
    (check-equal? (reduce (term boolean (and false (not false))))
                  (term boolean false))
    (check-equal? (reduce (term boolean (and (not false) (not false))))
                  (term boolean true))
    (check-equal? (reduce (term boolean (or true)))
                  (term boolean true))
    (check-equal? (reduce (term boolean (or false)))
                  (term boolean false))
    (check-equal? (reduce (term boolean (or true true)))
                  (term boolean true))
    (check-equal? (reduce (term boolean (or false true)))
                  (term boolean true))
    (check-equal? (reduce (term boolean (or true false)))
                  (term boolean true))
    (check-equal? (reduce (term boolean (or false false)))
                  (term boolean false))
    (check-equal? (reduce (term boolean (or false false true false true)))
                  (term boolean true)))

  (test-case "test"
    (check-equal? (reduce (term test (foo bar bar)))
                  (term test bar))
    (check-equal? (reduce (term test (foo bar (foo bar bar))))
                  (term test bar))
    (check-equal? (reduce (term test (foo bar baz)))
                  (term test (foo bar baz)))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests rewrite-tests))
