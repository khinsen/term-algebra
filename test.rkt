#lang racket

(module truth term-algebra
  (define-op true)
  (define-op false))

(module boolean term-algebra
  (define-op true)
  (define-op false)
  
  (define-op (not x))
  (define-op (and x y))
  (define-op (or x y))
    
  (=-> (not true) false)
  (=-> (not false) true)

  (define-vars X Y)

  (=-> (not (not X)) X)

  (=-> (and true true) true)
  (=-> (and false X) false)
  (=-> (and X false) false)
  (=-> (not (and X Y)) (or (not X) (not Y)))
      
  (=-> (or false false) false)
  (=-> (or true X) true)
  (=-> (or X true) true)
  (=-> (not (or X Y)) (and (not X) (not Y))))


(require term-algebra/client
         (only-in term-algebra/terms reduce))

(use-module boolean (submod "." boolean))

(define (test-for term)
  (display term)
  (display " -> ")
  (displayln (reduce term)))

(test-for (term boolean (and true true)))
