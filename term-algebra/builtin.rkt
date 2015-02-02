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

  (sorts Natural Zero NonZeroNatural
         Integer NonZeroInteger
         Rational NonZeroRational PositiveRational)

  (subsorts [Zero Natural] [NonZeroNatural Natural]

            [NonZeroInteger Integer]
            [Natural Integer] [NonZeroNatural NonZeroInteger]

            [NonZeroRational Rational] [PositiveRational NonZeroRational]
            [Integer Rational] [NonZeroInteger NonZeroRational]
            [NonZeroNatural PositiveRational])

  (special-ops rational-number)

  (op (+) Zero)
  (op (+ Zero ...) Zero)
  (op (+ Natural ...) Natural)
  (op (+ NonZeroNatural ...) NonZeroNatural)
  (op (+ Integer ...) Integer)
  (op (+ Rational ...) Rational)
  (op (+ PositiveRational ...) PositiveRational)
  (fn + +)

  (op (- Zero) Zero)
  (op (- Integer) Integer)
  (op (- Rational) Rational)
  (op (- Integer Integer) Integer)
  (op (- Rational Rational) Rational)
  (fn - -)

  (op (dec NonZeroNatural) Natural)
  (op (dec Integer) Integer)
  (op (dec Rational) Rational)
  (fn dec sub1)

  (op (*) Natural)
  (op (* Natural ...) Natural)
  (op (* Zero ...) Zero)
  (op (* NonZeroNatural ...) NonZeroNatural)
  (op (* Integer ...) Integer)
  (op (* NonZeroInteger ...) NonZeroInteger)
  (op (* Rational ...) Rational)
  (op (* NonZeroRational ...) NonZeroRational)
  (op (* PositiveRational ...) PositiveRational)
  (fn * *)

  (op (/ Rational) Rational)
  (op (/ PositiveRational) PositiveRational)
  (op (/ Rational Rational) Rational)
  (op (/ NonZeroRational NonZeroRational) NonZeroRational)
  (op (/ PositiveRational PositiveRational) PositiveRational)
  (fn / /)

  (op (div Integer NonZeroInteger) Integer)
  (fn div quotient)

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
  
