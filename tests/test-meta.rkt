#lang racket

(provide meta-tests)

(require rackunit
         (only-in term-algebra/syntax define-module term meta-term)
         (prefix-in meta: term-algebra/meta)
         (prefix-in builtin: term-algebra/builtin))

(define-module test
  (use builtin:equality)
  (sorts A B)
  (subsort A B)
  (op anA A)
  (op aB B)
  (op (foo B) B))

(define-test-suite meta-tests

  (test-case "meta-up-and-down"
    (define a-term (term test (foo aB)))
    (check-equal? (meta:meta-up a-term)
                  (meta-term (foo aB)))
    (check-equal? (meta:meta-down meta:m-term
                                  (meta:meta-up (meta:meta-up a-term)))
                  (meta:meta-up a-term))
    (check-equal? (meta:meta-down test
                                  (meta:meta-up a-term))
                  a-term))
  
  (test-exn "variable-not-in-pattern-1"
      #rx"Term.*contains variables that are not in the rule pattern"
    (lambda ()
      (define-module test2
        (include test)
        (=-> #:vars ([X A] [Y B]) (foo X) Y))
      (void)))
  
  (test-exn "variable-not-in-pattern-2"
      #rx"Condition.*contains variables that are not in the rule pattern"
    (lambda ()
      (define-module test2
        (include test)
        (=-> #:vars ([X A] [Y B]) (foo X) #:if (== X Y) X))
      (void)))
  
  (test-exn "condition-not-boolean"
      #rx"Condition.*not of sort Boolean"
    (lambda ()
      (define-module test2
        (include test)
        (=-> #:var [X A] (foo X) #:if X X))
      (void)))
  
  (test-exn "undefined-operator-in-rule"
      #rx"Undefined operator.*"
    (lambda ()
      (define-module test2
        (include test)
        (=-> #:var [X A] (bar X) (foo X)))
      (void)))
  
  (test-exn "too-many-vars"
      #rx"Var list contains variables .* that are not used in the rule"
    (lambda ()
      (define-module test
        (use builtin:truth)
        (=-> #:var [X Boolean] true false))
      (void)))

  (test-exn "svar-in-wrong-position"
      #rx"svar allowed only as last argument"
    (lambda ()
      (define-module test
        (sort A)
        (op (foo A ...) A)
        (=-> #:vars ([X A] [Y A ...])
             (foo Y X)
             (foo X)))
      (void)))
  (test-not-exn "svar-in-right-position"
    (lambda ()
      (define-module test
        (sort A)
        (op (foo A ...) A)
        (=-> #:vars ([X A] [Y A ...])
             (foo X Y)
             (foo Y)))
      (void)))

  (test-exn "no-rule-for-imported-op"
      #rx"Cannot add rule for operator .* imported in restricted mode"
    (lambda ()
      (define-module test
        (use builtin:truth)
        (=-> true false))
      (void)))
  (test-not-exn "rule-for-imported-op"
    (lambda ()
      (define-module test
        (include builtin:truth)
        (=-> true false))
      (void))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests meta-tests))
