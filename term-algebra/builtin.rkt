#lang racket

(provide any truth equality string symbol natural integer rational
         lookup-node)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in nodes: term-algebra/nodes)
         (prefix-in terms: term-algebra/terms))

(nodes:define-builtin-node truth
  (sorts Boolean)
  (op true Boolean)
  (op false Boolean))

(define true (terms:make-term 'true empty (nodes:node-ops truth)))
(define false (terms:make-term 'false empty (nodes:node-ops truth)))

(nodes:define-builtin-node any
  (sorts Any))

(nodes:define-builtin-node equality
  (use truth)
  (use any)
  (op (== Any Any) Boolean)
  (fn ==
    (lambda (term1 term2)
      (if (equal? term1 term2)
          true
          false))))

(nodes:define-builtin-node string
  (sorts String)
  (special-ops string))

(nodes:define-builtin-node symbol
  (sorts Symbol)
  (special-ops symbol))

(nodes:define-builtin-node natural
  (use truth)

  (sorts Natural Zero NonZeroNatural)
  (subsorts [Zero Natural] [NonZeroNatural Natural])

  (special-ops natural-number)

  (op (+ Natural ...) Natural)
  (op (+ Zero ...) Zero)
  (op (+ NonZeroNatural ...) NonZeroNatural)
  (fn + +)

  (op (dec NonZeroNatural) Natural)
  (fn dec sub1)

  (op (* Natural ...) Natural)
  (op (* Zero ...) Zero)
  (op (* NonZeroNatural ...) NonZeroNatural)
  (fn * *)

  (op (div Natural NonZeroNatural) Natural)
  (fn div quotient)

  (op (> Natural Natural) Boolean)
  (fn > (lambda (x y) (if (> x y) true false)))

  (op (>= Natural Natural) Boolean)
  (fn >= (lambda (x y) (if (>= x y) true false)))

  (op (< Natural Natural) Boolean)
  (fn < (lambda (x y) (if (< x y) true false)))

  (op (<= Natural Natural) Boolean)
  (fn <= (lambda (x y) (if (<= x y) true false)))

  (op (= Natural Natural) Boolean)
  (fn = (lambda (x y) (if (= x y) true false))))

(nodes:define-builtin-node integer
  (include natural)

  (sorts Integer NonZeroInteger)

  (subsorts [NonZeroInteger Integer]
            [Natural Integer] [NonZeroNatural NonZeroInteger])

  (special-ops integer-number)

  (op (+ Integer ...) Integer)

  (op (- Zero) Zero)
  (op (- Integer) Integer)
  (op (- Integer Integer) Integer)
  (fn - -)

  (op (dec Integer) Integer)

  (op (* Integer ...) Integer)
  (op (* NonZeroInteger ...) NonZeroInteger)

  (op (div Integer NonZeroInteger) Integer)

  (op (> Integer Integer) Boolean)
  (op (>= Integer Integer) Boolean)
  (op (< Integer Integer) Boolean)
  (op (<= Integer Integer) Boolean)
  (op (= Integer Integer) Boolean))

(nodes:define-builtin-node rational
  (include integer)

  (sorts Rational NonZeroRational PositiveRational)

  (subsorts [NonZeroRational Rational] [PositiveRational NonZeroRational]
            [Integer Rational] [NonZeroInteger NonZeroRational]
            [NonZeroNatural PositiveRational])

  (special-ops rational-number)

  (op (+ Rational ...) Rational)
  (op (+ PositiveRational ...) PositiveRational)

  (op (- Rational) Rational)
  (op (- Rational Rational) Rational)

  (op (dec Rational) Rational)

  (op (* Rational ...) Rational)
  (op (* NonZeroRational ...) NonZeroRational)
  (op (* PositiveRational ...) PositiveRational)

  (op (/ Rational) Rational)
  (op (/ PositiveRational) PositiveRational)
  (op (/ Rational Rational) Rational)
  (op (/ NonZeroRational NonZeroRational) NonZeroRational)
  (op (/ PositiveRational PositiveRational) PositiveRational)
  (fn / /)

  (op (> Rational Rational) Boolean)
  (op (>= Rational Rational) Boolean)
  (op (< Rational Rational) Boolean)
  (op (<= Rational Rational) Boolean)
  (op (= Rational Rational) Boolean))

;
; Lookup builtin node by name
;
(define (lookup-node name)
  (case name
    ['any any]
    ['truth truth]
    ['equality equality]
    ['string string]
    ['symbol symbol]
    ['natural natural]
    ['integer integer]
    ['rational rational]
    [else #f]))
