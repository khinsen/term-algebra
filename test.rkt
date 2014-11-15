#lang racket

(module peano-numbers term-algebra

  ; Constructors
  (define-op zero)
  (define-op (succ n))

  ; Error expression
  (define-op error)
  
  ; Function operators
  (define-vars N M)

  (define-op (pred n))
  (=-> (pred (succ N)) N)

  (define-op (+ a b))
  (=-> (+ N zero) N)
  (=-> (+ zero N) N)
  (=-> (+ N (succ M)) (+ (succ N) M))

  (define-op (- a b))
  (=-> (- N zero) N)
  (=-> (- zero (succ N)) error)
  (=-> (- (succ N) (succ M)) (- N M))
  
  (define-op (* a b))
  (=-> (* N zero) zero)
  (=-> (* zero N) zero)
  (=-> (* (succ N) (succ M)) (succ (+ N (+ M (* N M)))))

  (define-op (/ a b))
  (=-> (/ zero (succ N)) zero)
  (=-> (/ (succ N) zero) error)
  (=-> (/ (succ N) (succ M)) (succ (/ (- (succ N) (succ M)) (succ M))))

  (define-op (foo n))
  (=-> (foo N) zero #:if (== N (succ zero)))

  )

(require (only-in term-algebra/main term)
         (only-in term-algebra/terms reduce))

(define (test-for term)
  (display term)
  (display " -> ")
  (displayln (reduce term)))

(test-for (term (submod "." peano-numbers) (- (succ (succ zero)) (succ zero))))
(test-for (term (submod "." peano-numbers) (- zero (succ (succ zero)))))

(test-for (term (submod "." peano-numbers) (/ zero (succ zero))))
(test-for (term (submod "." peano-numbers) (/ (succ (succ (succ zero))) (succ (succ zero)))))

(test-for (term (submod "." peano-numbers) (foo zero)))
(test-for (term (submod "." peano-numbers) (foo (succ zero))))
(test-for (term (submod "." peano-numbers) (foo (succ (succ zero)))))

