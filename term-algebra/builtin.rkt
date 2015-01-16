#lang racket

(provide truth equality string symbol rational)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in modules: term-algebra/modules)
         (prefix-in terms: term-algebra/terms))

(modules:define-builtin-module truth
  (sorts Boolean)
  (op true Boolean)
  (op false Boolean))

(define true (terms:make-term 'true empty (modules:module-ops truth)))
(define false (terms:make-term 'false empty (modules:module-ops truth)))

(modules:define-builtin-module any
  (sorts Any))

(modules:define-builtin-module equality
  (use truth)
  (use any)
  (op (== Any Any) Boolean)
  (fn ==
    (lambda (term1 term2)
      (if (equal? term1 term2)
          true
          false))))

(modules:define-builtin-module string
  (sorts String)
  (special-ops string))

(modules:define-builtin-module symbol
  (sorts Symbol)
  (special-ops symbol))

(modules:define-builtin-module rational
  (use truth)
  (sorts Rational)
  (special-ops rational-number)
  (op (+ Rational Rational) Rational)
  (fn + (lambda (x y) (+ x y)))
  (op (- Rational Rational) Rational)
  (fn - (lambda (x y) (- x y)))
  (op (* Rational Rational) Rational)
  (fn * (lambda (x y) (* x y)))
  (op (/ Rational Rational) Rational)
  (fn / (lambda (x y) (/ x y)))
  (op (div Rational Rational) Rational)
  (fn div (lambda (x y) (quotient x y)))
  (op (> Rational Rational) Boolean)
  (fn > (lambda (x y) (if (> x y) true false)))
  (op (>= Rational Rational) Boolean)
  (fn >= (lambda (x y) (if (>= x y) true false)))
  (op (< Rational Rational) Boolean)
  (fn < (lambda (x y) (if (< x y) true false)))
  (op (<= Rational Rational) Boolean)
  (fn <= (lambda (x y) (if (<= x y) true false)))
  (op (= Rational Rational) Boolean)
  (fn = (lambda (x y) (if (= x y) true false))))
  
