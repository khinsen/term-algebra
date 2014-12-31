#lang racket

(provide peano-number-tests)

(require rackunit
         term-algebra/syntax
         term-algebra/rewrite)

(define-syntax-rule (test-reduce module initial-term reduced-term)
  (test-equal? (symbol->string (quote module))
               (reduce (term module initial-term))
               (term module reduced-term)))

(define-module pn
  (sort PN)

  (op zero PN)

  (op (succ PN) PN)

  (op (pred PN) PN)
  (=-> #:var [N PN] 
       (pred (succ N))
       N)

  (op (+ PN PN) PN)
  (=-> #:var [N PN]
       (+ N zero)
       N)
  (=-> #:var [N PN]
       (+ zero N)
       N)
  (=-> #:vars ([N PN] [M PN])
       (+ N (succ M))
       (+ (succ N) M))
  
  (op (* PN PN) PN)
  (=-> #:var [N PN]
       (* N zero)
       zero)
  (=-> #:var [N PN]
       (* zero N)
       zero)
  (=-> #:vars ([N PN] [M PN])
       (* (succ N) (succ M))
       (succ (+ N (+ M (* N M))))))

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
