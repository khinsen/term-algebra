#lang racket

(provide builtin-tests)

(require rackunit
         (only-in term-algebra/rewrite reduce)
         (only-in term-algebra/modules term define-module)
         term-algebra/builtin)

(define-syntax-rule (test-reduce module initial-term reduced-term)
  (test-equal? (symbol->string (quote module))
               (reduce (term module initial-term))
               (term module reduced-term)))

(define-test-suite builtin-tests

  (test-reduce equality (== false false)
                        true)
  (test-reduce equality (== true true)
                        true)
  (test-reduce equality (== false true)
                        false)
  (test-reduce equality (== true false)
                        false)
  (test-reduce equality (== (== true true) (== false false))
                        true)

  (test-reduce rational (+ 2 3)
                        5)
  (test-reduce rational (+ 2/3 1/3)
                        1)
  (test-reduce rational (- 2 3)
                        -1)
  (test-reduce rational (* 2 3)
                        6)
  (test-reduce rational (/ 2 3)
                        2/3)
  (test-reduce rational (/ 1 0)
                        (/ 1 0))
  (test-reduce rational (div 2 3)
                        0)
  (test-reduce rational (> 2 3)
                        false)
  (test-reduce rational (< 2 3)
                        true)
  (test-reduce rational (= 2 3)
                        false)
  (test-reduce rational (= 1/5 2/10)
                        true)

  (test-exn "too-many-vars"
            #rx"vars unused in left-hand-side.*"
            (lambda () (define-module test
                    (use truth)
                    (=-> #:var [X Boolean] true false))
               test))

  (test-exn "no-rule-for-imported-op"
            #rx"cannot add rule to imported operator.*"
            (lambda () (define-module test
                    (use truth)
                    (=-> true false))
               test))
  (test-not-exn "rule-for-imported-op"
            (lambda () (define-module test
                    (extend truth)
                    (=-> true false))
               test))

  (test-exn "undefined-range-sort"
            #rx"undefined sort.*"
            (lambda () (define-module test
                    (op foo Boolean))
               test))
  (test-exn "undefined-arg-sort"
            #rx"undefined sort.*"
            (lambda () (define-module test
                    (sort Bar)
                    (op (foo Boolean) Bar))
               test))

  (test-exn "sort-redefinition"
            #rx"sort already defined.*"
            (lambda () (define-module test
                    (sort Bar)
                    (sort Bar))
               test))

  (test-exn "undefined-sort-in-subsort"
            #rx"undefined sort.*"
            (lambda () (define-module test
                    (sort Bar)
                    (subsort Foo Bar))
               test))
  (test-exn "undefined-sort-in-subsort"
            #rx"undefined sort.*"
            (lambda () (define-module test
                    (sort Bar)
                    (subsort Bar Foo))
               test))
  (test-exn "equal-sorts-in-subsort"
            #rx"sorts are equal.*"
            (lambda () (define-module test
                    (sorts Foo Bar)
                    (subsort Foo Foo))
               test))
  (test-exn "cyclic-subsorts"
            #rx"cyclic subsort dependence.*"
            (lambda () (define-module test
                    (sorts Foo Bar Baz)
                    (subsort Foo Bar)
                    (subsort Bar Baz)
                    (subsort Baz Foo))
               test))
  (test-exn "subsort-redefinition"
            #rx"subsort relation already defined.*"
            (lambda () (define-module test
                    (sorts Foo Bar)
                    (subsort Foo Bar)
                    (subsort Foo Bar))
               test))

  (test-not-exn "string-imported"
            (lambda () (define-module test
                    (use string)
                    (op foo String)
                    (=-> foo "foo"))
               test))
  (test-exn "string-not-imported"
            #rx"import the string module.*"
            (lambda () (define-module test
                    (sort String)
                    (op foo String)
                    (=-> foo "foo"))
               test))

  (test-not-exn "symbol-imported"
            (lambda () (define-module test
                    (use symbol)
                    (op foo Symbol)
                    (=-> foo 'foo))
               test))
  (test-exn "symbol-not-imported"
            #rx"import the symbol module.*"
            (lambda () (define-module test
                    (sort Symbol)
                    (op foo Symbol)
                    (=-> foo 'foo))
               test))
  
  (test-not-exn "number-imported"
            (lambda () (define-module test
                    (use rational)
                    (op foo Rational)
                    (=-> foo 2))
               test))
  (test-exn "number-not-imported"
            #rx"import the rational module.*"
            (lambda () (define-module test
                    (sort Rational)
                    (op foo Rational)
                    (=-> foo 2))
               test))
  
  (test-exn "wrong-sort-in-term"
            #rx"sort String not compatible with Boolean"
            (lambda () (define-module test
                    (use truth)
                    (use string)
                    (op (not Boolean) Boolean))
              (term test (not "abc")))))


(module* main #f
  (require rackunit/text-ui)
  (run-tests builtin-tests))
