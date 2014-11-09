#lang racket

(require rackunit
         racket/splicing
         (only-in term-algebra/main term)
         (only-in term-algebra/terms reduce))

(define (check-reduce term reduced-term)
  (check-equal? (reduce term) reduced-term))

(test-case "peano-numbers"

  (local-require "./peano-numbers.rkt"
                 (prefix-in terms: term-algebra/terms))

  (define (peano n)
    (cond
     [(zero? n) (term zero)]
     [(positive? n) (terms:term succ (list (peano (- n 1))))]))
  
  (check-reduce (term (pred (succ zero)))
                (peano 0))

  (check-reduce (term (+ zero zero))
                (peano 0))
  (check-reduce (term (+ (succ zero) (succ zero)))
                (peano 2))
  (check-reduce (term (+ (succ zero) (succ (succ zero))))
                (peano 3))

  (check-reduce (term (* zero zero))
                (peano 0))
  (check-reduce (term (* (succ (succ zero)) (succ (succ (succ zero)))))
                (peano 6)))

