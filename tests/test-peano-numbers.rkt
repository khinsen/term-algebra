#lang racket

(provide peano-number-tests)

(require rackunit
         term-algebra/basic-api)

(define-syntax-rule (check-reduce node initial-term reduced-term)
  (check-equal? (reduce (term node initial-term))
                (term node reduced-term)))

(define-syntax-rule (check-normal node term1 term2)
  (check-equal? (reduce (term node term1))
                (reduce (term node term2))))

(define-node pn
  (sorts PN NZPN)
  (subsort NZPN PN)

  (op zero PN)
  (op (succ PN) NZPN)
  (op (pred NZPN) PN)

  (=> #:var [N PN] 
      (pred (succ N))
      N)

  (op (+ PN ...) PN #:symmetric)
  (=> #:var [N PN]
      (+ N)
      N)
  (=> #:var [N PN]
      (+ zero N)
      N)
  (=> #:vars ([N PN] [M PN])
      (+ N (succ M))
      (+ (succ N) M))
  (=> #:vars ([N PN] [M PN] [Ks PN ...])
      (+ N M Ks)
      (+ (+ N M) Ks))
  
  (op (* PN ...) PN #:symmetric)
  (=> #:var [N PN]
      (* N)
      N)
  (=> #:var [N PN]
      (* zero N)
      zero)
  (=> #:vars ([N PN] [M PN])
      (* (succ N) (succ M))
      (succ (+ N (+ M (* N M)))))
  (=> #:vars ([N PN] [M PN] [Ks PN ...])
      (* N M Ks)
      (* (* N M) Ks)))

(define-node pn+
  (use builtin:natural)
  (include pn)
  (op (from-natural Natural) PN)
  (=> (from-natural 0) zero)
  (=> #:var [N NonZeroNatural]
      (from-natural N)
      (succ (from-natural (dec N)))))

(define-test-suite peano-number-tests

  (test-case "peano-numbers"
    (check-reduce pn (pred (succ zero)) zero)

    (check-reduce pn (+ zero zero) zero)
    (check-reduce pn (+ zero (succ zero)) (succ zero))
    (check-reduce pn (+ (succ zero) zero) (succ zero))
    (check-reduce pn (+ (succ zero) (succ zero)) (succ (succ zero)))
    (check-reduce pn (+ (succ zero) (succ (succ zero)))
                     (succ (succ (succ zero))))

    (check-reduce pn (* zero zero) zero)
    (check-reduce pn (* zero (succ zero)) zero)
    (check-reduce pn (* (succ zero) zero) zero)
    (check-reduce pn (* (succ (succ zero)) (succ (succ (succ zero))))
                     (succ (succ (succ (succ (succ (succ zero))))))))
  
  (test-case "peano-number-conversion"
    (check-normal pn+ (+ (from-natural 4) (from-natural 5))
                      (from-natural 9))
    (check-normal pn+ (+ (from-natural 4) (from-natural 0))
                      (from-natural 4))
    (check-normal pn+ (+ (from-natural 3) (from-natural 2) (from-natural 1))
                      (from-natural 6))
    (check-normal pn+ (* (from-natural 2) (from-natural 3))
                      (from-natural 6))
    (check-normal pn+ (* (from-natural 0) (from-natural 3))
                      (from-natural 0))
    (check-normal pn+ (* (from-natural 2) (from-natural 0))
                      (from-natural 0))
    (check-normal pn+ (* (from-natural 2) (from-natural 1) (from-natural 2))
                      (from-natural 4))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests peano-number-tests))
