#lang racket

(provide boolean)

(require term-algebra/api)

(define-module boolean

  (use builtin:truth)
 
  (op (not Boolean) Boolean)
  (=-> (not true) false)
  (=-> (not false) true)

  (op (and Boolean Boolean) Boolean)
  (=-> (and true true) true)
  (=-> #:var [X Boolean] (and false X) false)
  (=-> #:var [X Boolean] (and X false) false)

  (op (or Boolean Boolean) Boolean)
  (=-> (or false false) false)
  (=-> #:var [X Boolean] (or true X) true)
  (=-> #:var [X Boolean] (or X true) true)
  
  (op (xor Boolean Boolean) Boolean)
  (=-> #:var [X Boolean] (xor false X) X)
  (=-> #:var [X Boolean] (xor X false) X)
  (=-> #:var [X Boolean] (xor true X) (not X))
  (=-> #:var [X Boolean] (xor X true) (not X)))
