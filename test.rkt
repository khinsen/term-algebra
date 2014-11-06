#lang racket

(module boolean term-algebra
  
  (include term-algebra/truth)
  
  (define-op (not x))
  (define-op (and x y))
  (define-op (or x y))
  
  (=-> (not true) false)
  (=-> (not false) true)

  (define-vars X Y)

  (=-> (not (not X)) X)

  (=-> (and false X) false)
  (=-> (and X false) false)
  (=-> (not (and X Y)) (or (not X) (not Y)))
    
  (=-> (or true X) true)
  (=-> (or X true) true)
  (=-> (not (or X Y)) (and (not X) (not Y))))

(module peano-numbers term-algebra

  (include (submod ".." boolean))
  
  (define-op zero)

  (define-vars N M)

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

  (define-op (shorten n))
  (define-op one)
  (define-op two)
  (define-op three)
  (define-op four)
  (define-op five)

  (=-> (shorten (succ zero)) one)
  (=-> (shorten (succ one)) two)
  (=-> (shorten (succ two)) three)
  (=-> (shorten (succ three)) four)
  (=-> (shorten (succ four)) five)
  (=-> (shorten (succ N)) (shorten (succ (shorten N)))))

(require 'peano-numbers
         (only-in term-algebra/main term)
         (only-in term-algebra/terms reduce))

(define (test-for term)
  (display term)
  (display " -> ")
  (displayln (reduce term)))

(test-for (term (succ zero)))
(test-for (term (pred (succ zero))))
(test-for (term (succ (pred (succ zero)))))
(test-for (term (+ (succ (succ zero)) (succ zero))))
(test-for (term (shorten (* (succ (succ zero)) (succ (succ zero))))))

(test-for (term (and (not true) (not false))))
