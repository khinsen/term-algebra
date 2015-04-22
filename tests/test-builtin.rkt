#lang racket

(provide builtin-tests)

(require rackunit
         term-algebra/basic-api)

(define-syntax-rule (check-reduce node initial-term reduced-term)
  (check-equal? (reduce (term node initial-term))
                (term node reduced-term)))

(define-node test1
  (use builtin:natural)
  (op foo Natural))

(define-node test2
  (use builtin:integer)
  (op foo Integer))

(define-node test3
  (use builtin:rational)
  (op foo Rational))

(define-test-suite builtin-tests

  (test-case "equality"
    (check-reduce builtin:equality (== false false) true)
    (check-reduce builtin:equality (== true true) true)
    (check-reduce builtin:equality (== false true) false)
    (check-reduce builtin:equality (== true false) false)
    (check-reduce builtin:equality (== (== true true) (== false false)) true))

  (test-case "natural"
    (check-reduce builtin:natural (+ 1) 1)
    (check-reduce builtin:natural (+ 2 3) 5)
    (check-reduce builtin:natural (dec 3) 2)
    (check-reduce builtin:natural (* 2) 2)
    (check-reduce builtin:natural (* 2 3) 6)
    (check-reduce builtin:natural (div 2 3) 0)
    (check-reduce builtin:natural (div 11 2) 5)
    (check-reduce builtin:natural (> 2 3) false)
    (check-reduce builtin:natural (>= 2 3) false)
    (check-reduce builtin:natural (< 2 3) true)
    (check-reduce builtin:natural (<= 2 3) true)
    (check-reduce builtin:natural (= 2 3) false))

  (test-case "integer"
    (check-reduce builtin:integer (+ -42) -42)
    (check-reduce builtin:integer (+ 2 -3) -1)
    (check-reduce builtin:integer (dec -4) -5)
    (check-reduce builtin:integer (- 2) -2)
    (check-reduce builtin:integer (- 2 3) -1)
    (check-reduce builtin:integer (* -2) -2)
    (check-reduce builtin:integer (* -1 -3) 3)
    (check-reduce builtin:integer (div -11 2) -5)
    (check-reduce builtin:integer (> 1 -1) true)
    (check-reduce builtin:integer (>= -1 -2) true)
    (check-reduce builtin:integer (< 2 -3) false)
    (check-reduce builtin:integer (<= 2 -3) false)
    (check-reduce builtin:integer (= -2 -2) true))

  (test-case "rational"
    (check-reduce builtin:rational (+ 1/5) 1/5)
    (check-reduce builtin:rational (+ 2/3 -4/3) -2/3)
    (check-reduce builtin:rational (+ 2/3 1/3) 1)
    (check-reduce builtin:rational (- -2/7) 2/7)
    (check-reduce builtin:rational (- 1 -4/5) 9/5)
    (check-reduce builtin:rational (dec 2/3) -1/3)
    (check-reduce builtin:rational (* 2/5) 2/5)
    (check-reduce builtin:rational (* 2/3 3) 2)
    (check-reduce builtin:rational (/ 2) 1/2)
    (check-reduce builtin:rational (/ -2 3) -2/3)
    (check-reduce builtin:rational (/ 1 0) (/ 1 0))
    (check-reduce builtin:rational (div 2 3) 0)
    (check-reduce builtin:rational (> 2/7 3/7) false)
    (check-reduce builtin:rational (>= 2/7 3/7) false)
    (check-reduce builtin:rational (< 2/7 3/7) true)
    (check-reduce builtin:rational (<= 2/7 3/7) true)
    (check-reduce builtin:rational (= 1/5 2/10) true))

  (test-case "naturals-and-terms"
    (check-reduce test1 (+ 2 3) 5)
    (check-reduce test1 (+ foo) foo)
    (check-reduce test1 (+ foo foo) (+ foo foo))
    (check-reduce test1 (+ 2 foo) (+ 2 foo))
    (check-reduce test1 (+ 2 3 foo) (+ 5 foo))
    (check-reduce test1 (* 2 3) 6)
    (check-reduce test1 (* foo) foo)
    (check-reduce test1 (* foo foo) (* foo foo))
    (check-reduce test1 (* 2 foo) (* 2 foo))
    (check-reduce test1 (* 2 3 foo) (* 6 foo)))

  (test-case "integers-and-terms"
    (check-reduce test2 (+ 2 -3) -1)
    (check-reduce test2 (+ foo) foo)
    (check-reduce test2 (+ 2 -2 foo) foo)
    (check-reduce test2 (+ foo foo) (+ foo foo))
    (check-reduce test2 (+ 2 -2 foo foo) (+ foo foo))
    (check-reduce test2 (+ 2 foo) (+ 2 foo))
    (check-reduce test2 (+ 2 3 foo) (+ 5 foo))
    (check-reduce test2 (* 2 -3) -6)
    (check-reduce test2 (* foo) foo)
    (check-reduce test2 (* foo foo) (* foo foo))
    (check-reduce test2 (* 2 foo) (* 2 foo))
    (check-reduce test2 (* 2 -3 foo) (* -6 foo)))

  (test-case "rationale-and-terms"
    (check-reduce test3 (+ 2/3 1/3) 1)
    (check-reduce test3 (+ foo) foo)
    (check-reduce test3 (+ 2/3 -2/3 foo) foo)
    (check-reduce test3 (+ foo foo) (+ foo foo))
    (check-reduce test3 (+ 2/3 -2/3 foo foo) (+ foo foo))
    (check-reduce test3 (+ 1/2 foo) (+ 1/2 foo))
    (check-reduce test3 (+ 2/3 1/3 foo) (+ 1 foo))
    (check-reduce test3 (* 2 1/3) 2/3)
    (check-reduce test3 (* foo) foo)
    (check-reduce test3 (* 2 1/2 foo) foo)
    (check-reduce test3 (* foo foo) (* foo foo))
    (check-reduce test3 (* 2 1/2 foo foo) (* foo foo))
    (check-reduce test3 (* 2 foo) (* 2 foo))
    (check-reduce test3(* 2 1/3 foo) (* 2/3 foo)))

  (test-not-exn "string-imported"
    (lambda ()
      (define-node test
        (use builtin:string)
        (op foo String)
        (=-> foo "foo"))
      (void)))
  (test-exn "string-not-imported"
      #rx"import builtin:string to use strings"
    (lambda ()
      (define-node test
        (sort String)
        (op foo String)
        (=-> foo "foo"))
      (void)))

  (test-not-exn "symbol-imported"
    (lambda ()
      (define-node test
        (use builtin:symbol)
        (op foo Symbol)
        (=-> foo 'foo))
      (void)))
  (test-exn "symbol-not-imported"
      #rx"import builtin:symbol to use symbols"
    (lambda ()
      (define-node test
        (sort Symbol)
        (op foo Symbol)
        (=-> foo 'foo))
      (void)))

  (test-not-exn "natural-imported"
    (lambda ()
      (define-node test
        (use builtin:natural)
        (op foo Natural)
        (=-> foo 2))
      (void)))
  (test-not-exn "integer-imported"
    (lambda ()
      (define-node test
        (use builtin:integer)
        (op foo Integer)
        (=-> foo -5))
      (void)))
  (test-not-exn "rational-imported"
    (lambda ()
      (define-node test
        (use builtin:rational)
        (op foo Rational)
        (=-> foo 2/3))
      (void)))
  (test-exn "natural-not-imported"
      #rx"import builtin:natural to use natural numbers"
    (lambda ()
      (define-node test
        (sort Natural)
        (op foo Natural)
        (=-> foo 2))
      (void)))
  (test-exn "integer-not-imported"
      #rx"import builtin:integer to use integer numbers"
    (lambda ()
      (define-node test
        (sort Integer)
        (op foo Integer)
        (=-> foo -2))
      (void)))
  (test-exn "rational-not-imported"
      #rx"import builtin:rational to use rational numbers"
    (lambda ()
      (define-node test
        (sort Rational)
        (op foo Rational)
        (=-> foo 2/3))
      (void)))

  (test-exn "wrong-sort-in-term"
      #rx"Wrong number or sort of arguments.*"
    (lambda ()
      (define-node test
        (use builtin:truth)
        (use builtin:string)
        (op (not Boolean) Boolean))
      (term test (not "abc")))))


(module* main #f
  (require rackunit/text-ui)
  (run-tests builtin-tests))
