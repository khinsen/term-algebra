#lang racket

(provide meta-tests)

(require rackunit
         term-algebra/basic-api)

(define-node test
  (use builtin:equality)
  (sorts A B)
  (subsort A B)
  (op anA A)
  (op aB B)
  (op (foo B) B))

(define-test-suite meta-tests

  (test-case "meta-up-and-down"
    (define a-term (term test (foo aB)))
    (check-equal? (meta-up a-term)
                  (meta-term (foo aB)))
    (check-equal? (meta-down builtin:term (meta-up (meta-up a-term)))
                  (meta-up a-term))
    (check-equal? (meta-down test (meta-up a-term))
                  a-term))
  
  (test-exn "variable-not-in-pattern-1"
      #rx"Term.*contains variables that are not in the rule pattern"
    (lambda ()
      (define-node test2
        (include test)
        (=> #:vars ([X A] [Y B]) (foo X) Y))
      (void)))
  (test-exn "variable-not-in-pattern-2"
      #rx"Condition.*contains variables that are not in the rule pattern"
    (lambda ()
      (define-node test2
        (include test)
        (=> #:vars ([X A] [Y B]) (foo X) #:if (== X Y) X))
      (void)))
  (test-exn "variable-not-in-pattern-3"
      #rx"Var list contains variables.*that are not used in the rule"
    (lambda ()
      (define-node test2
        (include test)
        (=> #:vars ([X A] [Y B]) (foo X) X))
      (void)))
  (test-exn "variable-not-in-pattern-4"
      #rx"Condition.*contains variables that are not in the left or right pattern"
    (lambda ()
      (define-node test2
        (include test)
        (= #:vars ([X A] [Y B]) (foo X) X #:if (== X Y)))
      (void)))
  (test-exn "variable-not-in-pattern-5"
      #rx"Var list contains variables.*that are not used in the equation"
    (lambda ()
      (define-node test2
        (include test)
        (= #:vars ([X A] [Y B]) (foo X) X))
      (void)))
  
  (test-exn "condition-not-boolean-1"
      #rx"Condition.*not of sort Boolean"
    (lambda ()
      (define-node test2
        (include test)
        (=> #:var [X A] (foo X) #:if X X))
      (void)))
  (test-exn "condition-not-boolean-2"
      #rx"Condition.*not of sort Boolean"
    (lambda ()
      (define-node test2
        (include test)
        (= #:var [X A] (foo X) X #:if X))
      (void)))

  (test-exn "sort-mismatch-1"
      #rx"Term.*must be of sort.*"
    (lambda ()
      (define-node test2
        (include test)
        (sort C)
        (op aC C)
        (=> #:var [X A] (foo X) aC))
      (void)))
  (test-exn "sort-mismatch-2"
      #rx"Term.*and.*must be of the same kind"
    (lambda ()
      (define-node test2
        (include test)
        (sort C)
        (op aC C)
        (= #:var [X A] (foo X) aC))
      (void)))

  (test-exn "undefined-operator-in-rule"
      #rx"Undefined operator.*"
    (lambda ()
      (define-node test2
        (include test)
        (=> #:var [X A] (bar X) (foo X)))
      (void)))
  
  (test-exn "too-many-vars"
      #rx"Var list contains variables .* that are not used in the rule"
    (lambda ()
      (define-node test
        (use builtin:truth)
        (=> #:var [X Boolean] true false))
      (void)))

  (test-exn "svar-in-wrong-position"
      #rx"svar allowed only as last argument"
    (lambda ()
      (define-node test
        (sort A)
        (op (foo A ...) A)
        (=> #:vars ([X A] [Y A ...])
             (foo Y X)
             (foo X)))
      (void)))
  (test-not-exn "svar-in-right-position"
    (lambda ()
      (define-node test
        (sort A)
        (op (foo A ...) A)
        (=> #:vars ([X A] [Y A ...])
             (foo X Y)
             (foo Y)))
      (void)))

  (test-exn "no-rule-for-imported-op"
      #rx"Cannot add rule for operator .* imported in restricted mode"
    (lambda ()
      (define-node test
        (use builtin:truth)
        (=> true false))
      (void)))
  (test-not-exn "rule-for-imported-op"
    (lambda ()
      (define-node test
        (include builtin:truth)
        (=> true false))
      (void)))
  
  (test-exn "sort redefinition"
      #rx"sort already defined.*"
    (lambda ()
      (define-node test
        (sorts a b b c))
      (void)))
  (test-exn "equal-sorts-in-subsort"
      #rx"sorts are equal.*"
    (lambda ()
      (define-node test
        (sorts a b)
        (subsorts [a a]))
      (void)))
  (test-exn "double subsort definition"
      #rx"subsort relation already defined.*"
    (lambda ()
      (define-node test
        (sorts a b)
        (subsorts [a b] [a b]))
      (void)))
  (test-exn "double-definition"
      #rx"Signature .* already defined"
    (lambda ()
      (define-node test
        (sorts A B C X Y Z)
        (subsorts [A C] [B C] [X Y] [X Z])
        (op (foo A) X)
        (op (foo C) Z)
        (op (foo A) X))
      (void))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests meta-tests))
