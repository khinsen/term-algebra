#lang term-algebra
  
(include term-algebra/truth)
  
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
(=-> (not (or X Y)) (and (not X) (not Y)))
