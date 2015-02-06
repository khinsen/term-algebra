#lang racket

(provide boolean)

(require term-algebra/api)

(define-module boolean

  (use builtin:truth)
 
  (op (not Boolean) Boolean)
  (=-> (not true) false)
  (=-> (not false) true)

  (op (and Boolean ...) Boolean)
  (=-> #:var [X Boolean] (and X) X)
  (=-> #:var [X Boolean] (and false : X) false)
  (=-> #:var [X Boolean] (and true  : X) X)

  (op (or Boolean ...) Boolean)
  (=-> #:var [X Boolean] (or X) X)
  (=-> #:var [X Boolean] (or true  : X) true)
  (=-> #:var [X Boolean] (or false : X) X)
  
  (op (xor Boolean ...) Boolean)
  (=-> #:var [X Boolean] (xor X) X)
  (=-> #:var [X Boolean] (xor false : X) X)
  (=-> #:var [X Boolean] (xor true  : X) (not X)))
