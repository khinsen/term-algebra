#lang racket

(provide peano-number-tests)

(require rackunit
         term-algebra/modules
         term-algebra/rewrite)

(define-syntax-rule (test-reduce module initial-term reduced-term)
  (test-equal? (symbol->string (quote module))
               (reduce (term module initial-term))
               (term module reduced-term)))

(define-module pn
  (op zero)

  (op (succ n))

  (op (pred n))
  (=-> #:var N (pred (succ N)) N)

  (op (+ a b))
  (=-> #:var N (+ N zero) N)
  (=-> #:var N (+ zero N) N)
  (=-> #:vars (N M) (+ N (succ M)) (+ (succ N) M))
  
  (op (* a b))
  (=-> #:var N (* N zero) zero)
  (=-> #:var N (* zero N) zero)
  (=-> #:vars (N M) (* (succ N) (succ M)) (succ (+ N (+ M (* N M))))))

(define-test-suite peano-number-tests

  (test-reduce pn (pred (succ zero))
                  zero)

  (test-reduce pn (+ zero zero)
                  zero)
  (test-reduce pn (+ (succ zero) (succ zero))
                  (succ (succ zero)))
  (test-reduce pn (+ (succ zero) (succ (succ zero)))
                  (succ (succ (succ zero))))

  (test-reduce pn (* zero zero)
                  zero)
  (test-reduce pn (* (succ (succ zero)) (succ (succ (succ zero))))
                  (succ (succ (succ (succ (succ (succ zero))))))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests peano-number-tests))
