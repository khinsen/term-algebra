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
    (λ (term)
      (let ([args (terms:term-args term)])
        (if (equal? (first args) (second args))
            true
            false)))))

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

  (op (+ Natural ...) Natural #:symmetric)
  (op (+ Zero ...) Zero  #:symmetric)
  (op (+ NonZeroNatural ...) NonZeroNatural  #:symmetric)
  (fn + (λ (term)
          (unless (andmap (λ (a) (is-sort? a 'Natural)) (terms:term-args term))
            (error 'not-my-domain))
          (let*-values ([(numbers subterms)
                         (partition number? (terms:term-args term))]
                        [(sum-of-numbers)
                         (foldl + 0 numbers)])
            (cond
              [(empty? subterms) sum-of-numbers]
              [(zero? sum-of-numbers)
               (cond
                 [(= 1 (length subterms))
                  (first subterms)]
                 [(empty? numbers)
                  term]
                 [else
                  (make-term '+ subterms)])]
              [(= (length numbers) 1) term]
              [else (make-term '+ (cons sum-of-numbers subterms))]))))

  (op (dec NonZeroNatural) Natural)
  (fn dec (λ (term) (sub1 (first (terms:term-args term)))))

  (op (* Natural ...) Natural #:symmetric)
  (op (* Zero ...) Zero #:symmetric)
  (op (* NonZeroNatural ...) NonZeroNatural #:symmetric)
  (fn * (λ (term)
          (unless (andmap (λ (a) (is-sort? a 'Natural)) (terms:term-args term))
            (error 'not-my-domain))
          (let*-values ([(numbers subterms)
                         (partition number? (terms:term-args term))]
                        [(product-of-numbers)
                         (foldl * 1 numbers)])
            (cond
              [(empty? subterms) product-of-numbers]
              [(= 1 product-of-numbers)
               (cond
                 [(= 1 (length subterms))
                  (first subterms)]
                 [(empty? numbers)
                  term]
                 [else
                  (make-term '* subterms)])]
              [(= (length numbers) 1) term]
              [else (make-term '* (cons product-of-numbers subterms))]))))

  (op (div Natural NonZeroNatural) Natural)
  (fn div (λ (term)
            (let ([args (terms:term-args term)])
              (quotient (first args) (second args)))))

  (op (> Natural Natural) Boolean)
  (fn > (λ (term)
          (let ([args (terms:term-args term)])
            (if (> (first args) (second args))
                true
                false))))

  (op (>= Natural Natural) Boolean)
  (fn >= (λ (term)
           (let ([args (terms:term-args term)])
             (if (>= (first args) (second args))
                 true
                 false))))

  (op (< Natural Natural) Boolean)
  (fn < (λ (term)
          (let ([args (terms:term-args term)])
            (if (< (first args) (second args))
                true
                false))))

  (op (<= Natural Natural) Boolean)
  (fn <= (λ (term)
           (let ([args (terms:term-args term)])
             (if (<= (first args) (second args))
                 true
                 false))))

  (op (= Natural Natural) Boolean)
  (fn = (λ (term)
          (let ([args (terms:term-args term)])
            (if (= (first args) (second args))
                true
                false)))))

(nodes:define-builtin-node integer
  (include natural)

  (sorts Integer NonZeroInteger)

  (subsorts [NonZeroInteger Integer]
            [Natural Integer] [NonZeroNatural NonZeroInteger])

  (special-ops integer-number)

  (op (+ Integer ...) Integer #:symmetric)
  (fn + (λ (term)
          (unless (andmap (λ (a) (is-sort? a 'Integer)) (terms:term-args term))
            (error 'not-my-domain))
          (let*-values ([(numbers subterms)
                         (partition number? (terms:term-args term))]
                        [(sum-of-numbers)
                         (foldl + 0 numbers)])
            (cond
              [(empty? subterms) sum-of-numbers]
              [(zero? sum-of-numbers)
               (cond
                 [(= 1 (length subterms))
                  (first subterms)]
                 [(empty? numbers)
                  term]
                 [else
                  (make-term '+ subterms)])]
              [(= (length numbers) 1) term]
              [else (make-term '+ (cons sum-of-numbers subterms))]))))

  (op (- Zero) Zero)
  (op (- Integer) Integer)
  (op (- Integer Integer) Integer)
  (fn - (λ (term) (apply - (terms:term-args term))))

  (op (dec Integer) Integer)

  (op (* Integer ...) Integer #:symmetric)
  (op (* NonZeroInteger ...) NonZeroInteger #:symmetric)
  (fn * (λ (term)
          (unless (andmap (λ (a) (is-sort? a 'Integer)) (terms:term-args term))
            (error 'not-my-domain))
          (let*-values ([(numbers subterms)
                         (partition number? (terms:term-args term))]
                        [(product-of-numbers)
                         (foldl * 1 numbers)])
            (cond
              [(empty? subterms) product-of-numbers]
              [(= 1 product-of-numbers)
               (cond
                 [(= 1 (length subterms))
                  (first subterms)]
                 [(empty? numbers)
                  term]
                 [else
                  (make-term '* subterms)])]
              [(= (length numbers) 1) term]
              [else (make-term '* (cons product-of-numbers subterms))]))))

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

  (op (+ Rational ...) Rational #:symmetric)
  (op (+ PositiveRational ...) PositiveRational #:symmetric)
  (fn + (λ (term)
          (unless (andmap (λ (a) (is-sort? a 'Rational)) (terms:term-args term))
            (error 'not-my-domain))
          (let*-values ([(numbers subterms)
                         (partition number? (terms:term-args term))]
                        [(sum-of-numbers)
                         (foldl + 0 numbers)])
            (cond
              [(empty? subterms) sum-of-numbers]
              [(zero? sum-of-numbers)
               (cond
                 [(= 1 (length subterms))
                  (first subterms)]
                 [(empty? numbers)
                  term]
                 [else
                  (make-term '+ subterms)])]
              [(= (length numbers) 1) term]
              [else (make-term '+ (cons sum-of-numbers subterms))]))))

  (op (- Rational) Rational)
  (op (- Rational Rational) Rational)

  (op (dec Rational) Rational)

  (op (* Rational ...) Rational #:symmetric)
  (op (* NonZeroRational ...) NonZeroRational #:symmetric)
  (op (* PositiveRational ...) PositiveRational #:symmetric)
  (fn * (λ (term)
          (unless (andmap (λ (a) (is-sort? a 'Rational)) (terms:term-args term))
            (error 'not-my-domain))
          (let*-values ([(numbers subterms)
                         (partition number? (terms:term-args term))]
                        [(product-of-numbers)
                         (foldl * 1 numbers)])
            (cond
              [(empty? subterms) product-of-numbers]
              [(= 1 product-of-numbers)
               (cond
                 [(= 1 (length subterms))
                  (first subterms)]
                 [(empty? numbers)
                  term]
                 [else
                  (make-term '* subterms)])]
              [(= (length numbers) 1) term]
              [else (make-term '* (cons product-of-numbers subterms))]))))

  (op (/ Rational) Rational)
  (op (/ PositiveRational) PositiveRational)
  (op (/ Rational Rational) Rational)
  (op (/ NonZeroRational NonZeroRational) NonZeroRational)
  (op (/ PositiveRational PositiveRational) PositiveRational)
  (fn / (λ (term) (apply / (terms:term-args term))))

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
