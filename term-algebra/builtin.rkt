#lang racket

(provide truth equality string symbol rational
         meta-term meta-module)

(require (prefix-in terms: term-algebra/terms)
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in modules: term-algebra/modules))

(define meta-term modules:metalevel-term)
(define meta-module modules:metalevel-module)

(modules:define-builtin-module truth
  (sorts Boolean)
  (op true Boolean)
  (op false Boolean))

(define true (terms:term (modules:op-from truth 'true) '()))
(define false (terms:term (modules:op-from truth 'false) '()))

(modules:define-builtin-module any
  (sorts Any))

(modules:define-builtin-module equality
  (use truth)
  (use any)
  (op (== Any Any) Boolean
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
  (op (+ Rational Rational) Rational
      (lambda (x y) (+ x y)))
  (op (- Rational Rational) Rational
      (lambda (x y) (- x y)))
  (op (* Rational Rational) Rational
      (lambda (x y) (* x y)))
  (op (/ Rational Rational) Rational
      (lambda (x y) (/ x y)))
  (op (div Rational Rational) Rational
      (lambda (x y) (quotient x y)))
  (op (> Rational Rational) Boolean
      (lambda (x y) (if (> x y) true false)))
  (op (>= Rational Rational) Boolean
      (lambda (x y) (if (>= x y) true false)))
  (op (< Rational Rational) Boolean
      (lambda (x y) (if (< x y) true false)))
  (op (<= Rational Rational) Boolean
      (lambda (x y) (if (<= x y) true false)))
  (op (= Rational Rational) Boolean
      (lambda (x y) (if (= x y) true false))))
