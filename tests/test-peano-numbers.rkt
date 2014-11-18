#lang racket

(require rackunit
         term-algebra/client
         (only-in term-algebra/terms reduce))

(define (check-reduce term reduced-term)
  (check-equal? (reduce term) reduced-term))

(test-case "peano-numbers"

  (use-module pn "./peano-numbers.rkt")

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

