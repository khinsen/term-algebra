#lang racket

(provide boolean)

(require term-algebra/basic-api)

(define-module boolean

  (use builtin:truth)
 
  (op (not Boolean) Boolean)
  (=-> (not true) false)
  (=-> (not false) true)

  (op (and Boolean ...) Boolean)
  (=-> #:vars ([X Boolean]) (and X) X)
  (=-> #:vars ([Xs Boolean ...]) (and false Xs) false)
  (=-> #:vars ([Xs Boolean ...]) (and true  Xs) (and Xs))

  (op (or Boolean ...) Boolean)
  (=-> #:vars ([X Boolean]) (or X) X)
  (=-> #:vars ([Xs Boolean ...]) (or true  Xs) true)
  (=-> #:vars ([Xs Boolean ...]) (or false Xs) (or Xs))
  
  (op (xor Boolean ...) Boolean)
  (=-> #:vars ([X Boolean]) (xor X) X)
  (=-> #:vars ([Xs Boolean ...]) (xor false Xs) (xor Xs))
  (=-> #:vars ([Xs Boolean ...]) (xor true  Xs) (not (xor Xs))))
