#lang term-algebra

(define-vars N M)

(define-op zero)

(define-op (succ n))

(define-op (pred n))
(=-> (pred (succ N)) N)

(define-op (+ a b))
(=-> (+ N zero) N)
(=-> (+ zero N) N)
(=-> (+ N (succ M)) (+ (succ N) M))

(define-op (* a b))
(=-> (* N zero) zero)
(=-> (* zero N) zero)
(=-> (* (succ N) (succ M)) (succ (+ N (+ M (* N M)))))
