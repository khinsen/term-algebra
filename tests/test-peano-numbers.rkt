#lang racket

(provide peano-number-tests)

(require rackunit
         term-algebra/api)

(define-syntax-rule (check-reduce module initial-term reduced-term)
  (check-equal? (reduce (term module initial-term))
                (term module reduced-term)))

(define-syntax-rule (check-normal module term1 term2)
  (check-equal? (reduce (term module term1))
                (reduce (term module term2))))

(define-module pn
  (sorts PN NZPN)
  (subsort NZPN PN)

  (op zero PN)
  (op (succ PN) NZPN)
  (op (pred NZPN) PN)

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

(define-module pn+
  (use builtin:rational)
  (include pn)
  (op (from-natural Natural) PN)
  (=-> (from-natural 0) zero)
  (=-> #:var [N NonZeroNatural]
       (from-natural N)
       (succ (from-natural (dec N)))))

(define-test-suite peano-number-tests

  (test-case "peano-numbers"
    (check-reduce pn (pred (succ zero)) zero)

    (check-reduce pn (+ zero zero) zero)
    (check-reduce pn (+ (succ zero) (succ zero)) (succ (succ zero)))
    (check-reduce pn (+ (succ zero) (succ (succ zero)))
                     (succ (succ (succ zero))))

    (check-reduce pn (* zero zero) zero)
    (check-reduce pn (* (succ (succ zero)) (succ (succ (succ zero))))
                     (succ (succ (succ (succ (succ (succ zero))))))))
  
  (test-case "peano-number-conversion"
    (check-normal pn+ (+ (from-natural 4) (from-natural 5))
                      (from-natural 9))
    (check-normal pn+ (+ (from-natural 4) (from-natural 0))
                      (from-natural 4))
    (check-normal pn+ (* (from-natural 2) (from-natural 3))
                      (from-natural 6))
    (check-normal pn+ (* (from-natural 0) (from-natural 3))
                      (from-natural 0))
    (check-normal pn+ (* (from-natural 2) (from-natural 0))
                      (from-natural 0))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests peano-number-tests))
