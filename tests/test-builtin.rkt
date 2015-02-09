#lang racket

(provide builtin-tests)

(require rackunit
         term-algebra/api
         term-algebra/builtin)

(define-syntax-rule (check-reduce module initial-term reduced-term)
  (check-equal? (reduce (term module initial-term))
                (term module reduced-term)))

(define-test-suite builtin-tests

  (test-case "equality"
    (check-reduce equality (== false false) true)
    (check-reduce equality (== true true) true)
    (check-reduce equality (== false true) false)
    (check-reduce equality (== true false) false)
    (check-reduce equality (== (== true true) (== false false)) true))

  (test-case "natural"
    (check-reduce natural (+ 1) 1)
    (check-reduce natural (+ 2 3) 5)
    (check-reduce natural (dec 3) 2)
    (check-reduce natural (* 2) 2)
    (check-reduce natural (* 2 3) 6)
    (check-reduce natural (div 2 3) 0)
    (check-reduce natural (div 11 2) 5)
    (check-reduce natural (> 2 3) false)
    (check-reduce natural (>= 2 3) false)
    (check-reduce natural (< 2 3) true)
    (check-reduce natural (<= 2 3) true)
    (check-reduce natural (= 2 3) false))

  (test-case "integer"
    (check-reduce integer (+ -42) -42)
    (check-reduce integer (+ 2 -3) -1)
    (check-reduce integer (dec -4) -5)
    (check-reduce integer (- 2) -2)
    (check-reduce integer (- 2 3) -1)
    (check-reduce integer (* -2) -2)
    (check-reduce integer (* -1 -3) 3)
    (check-reduce integer (div -11 2) -5)
    (check-reduce integer (> 1 -1) true)
    (check-reduce integer (>= -1 -2) true)
    (check-reduce integer (< 2 -3) false)
    (check-reduce integer (<= 2 -3) false)
    (check-reduce integer (= -2 -2) true))

  (test-case "rational"
    (check-reduce rational (+ 1/5) 1/5)
    (check-reduce rational (+ 2/3 -4/3) -2/3)
    (check-reduce rational (+ 2/3 1/3) 1)
    (check-reduce rational (- -2/7) 2/7)
    (check-reduce rational (- 1 -4/5) 9/5)
    (check-reduce rational (dec 2/3) -1/3)
    (check-reduce rational (* 2/5) 2/5)
    (check-reduce rational (* 2/3 3) 2)
    (check-reduce rational (/ 2) 1/2)
    (check-reduce rational (/ -2 3) -2/3)
    (check-reduce rational (/ 1 0) (/ 1 0))
    (check-reduce rational (div 2 3) 0)
    (check-reduce rational (> 2/7 3/7) false)
    (check-reduce rational (>= 2/7 3/7) false)
    (check-reduce rational (< 2/7 3/7) true)
    (check-reduce rational (<= 2/7 3/7) true)
    (check-reduce rational (= 1/5 2/10) true))

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

  (test-not-exn "natural-imported"
    (lambda ()
      (define-module test
        (use natural)
        (op foo Natural)
        (=-> foo 2))
      (void)))
  (test-not-exn "integer-imported"
    (lambda ()
      (define-module test
        (use integer)
        (op foo Integer)
        (=-> foo -5))
      (void)))
  (test-not-exn "rational-imported"
    (lambda ()
      (define-module test
        (use rational)
        (op foo Rational)
        (=-> foo 2/3))
      (void)))
  (test-exn "natural-not-imported"
      #rx"import builtin:natural to use natural numbers"
    (lambda ()
      (define-module test
        (sort Natural)
        (op foo Natural)
        (=-> foo 2))
      (void)))
  (test-exn "integer-not-imported"
      #rx"import builtin:integer to use integer numbers"
    (lambda ()
      (define-module test
        (sort Integer)
        (op foo Integer)
        (=-> foo -2))
      (void)))
  (test-exn "rational-not-imported"
      #rx"import builtin:rational to use rational numbers"
    (lambda ()
      (define-module test
        (sort Rational)
        (op foo Rational)
        (=-> foo 2/3))
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
