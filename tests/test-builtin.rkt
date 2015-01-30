#lang racket

(provide builtin-tests)

(require rackunit
         (only-in term-algebra/rewrite reduce)
         (only-in term-algebra/syntax define-module term)
         term-algebra/builtin)

(define-syntax-rule (test-reduce module initial-term reduced-term)
  (test-equal? (symbol->string (quote module))
               (reduce (term module initial-term) module)
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

  (test-reduce rational (+)
                        0)
  (test-reduce rational (+ 1)
                        1)
  (test-reduce rational (+ 2 3)
                        5)
  (test-reduce rational (+ 2/3 1/3)
                        1)

  (test-reduce rational (- 2)
                        -2)
  (test-reduce rational (- 2 3)
                        -1)

  (test-reduce rational (*)
                        1)
  (test-reduce rational (* 2)
                        2)
  (test-reduce rational (* 2 3)
                        6)

  (test-reduce rational (/ 2)
                        1/2)
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

  (test-not-exn "string-imported"
    (lambda ()
      (define-module test
        (use string)
        (op foo String)
        (=-> foo "foo"))
      (void)))
  (test-exn "string-not-imported"
      #rx"import builtin:string to use strings"
    (lambda ()
      (define-module test
        (sort String)
        (op foo String)
        (=-> foo "foo"))
      (void)))

  (test-not-exn "symbol-imported"
    (lambda ()
      (define-module test
        (use symbol)
        (op foo Symbol)
        (=-> foo 'foo))
      (void)))
  (test-exn "symbol-not-imported"
      #rx"import builtin:symbol to use symbols"
    (lambda ()
      (define-module test
        (sort Symbol)
        (op foo Symbol)
        (=-> foo 'foo))
      (void)))
  
  (test-not-exn "number-imported"
    (lambda ()
      (define-module test
        (use rational)
        (op foo Rational)
        (=-> foo 2))
      (void)))
  (test-exn "number-not-imported"
      #rx"import builtin:rational to use rational numbers"
    (lambda ()
      (define-module test
        (sort Rational)
        (op foo Rational)
        (=-> foo 2))
      (void)))
  
  (test-exn "wrong-sort-in-term"
      #rx"Wrong number or sort of arguments.*"
    (lambda ()
      (define-module test
        (use truth)
        (use string)
        (op (not Boolean) Boolean))
      (term test (not "abc")))))


(module* main #f
  (require rackunit/text-ui)
  (run-tests builtin-tests))
