#lang racket

(require rackunit
         term-algebra/modules
         (only-in term-algebra/terms reduce))

(define (check-reduce term reduced-term)
  (check-equal? (reduce term) reduced-term))

(define-module pn
  (define-op zero)

  (define-op (succ n))

  (define-op (pred n))
  (=-> #:var N (pred (succ N)) N)

  (define-op (+ a b))
  (=-> #:var N (+ N zero) N)
  (=-> #:var N (+ zero N) N)
  (=-> #:vars (N M) (+ N (succ M)) (+ (succ N) M))
  
  (define-op (* a b))
  (=-> #:var N (* N zero) zero)
  (=-> #:var N (* zero N) zero)
  (=-> #:vars (N M) (* (succ N) (succ M)) (succ (+ N (+ M (* N M))))))

(test-case "peano-numbers"

  (check-reduce (term pn (pred (succ zero)))
                (term pn zero))

  (check-reduce (term pn (+ zero zero))
                (term pn zero))
  (check-reduce (term pn (+ (succ zero) (succ zero)))
                (term pn (succ (succ zero))))
  (check-reduce (term pn (+ (succ zero) (succ (succ zero))))
                (term pn (succ (succ (succ zero)))))

  (check-reduce (term pn (* zero zero))
                (term pn zero))
  (check-reduce (term pn (* (succ (succ zero)) (succ (succ (succ zero)))))
                (term pn (succ (succ (succ (succ (succ (succ zero)))))))))

