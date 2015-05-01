#lang racket

(provide boolean)

(require term-algebra/basic-api)

(define-node boolean

  (use builtin:truth)
 
  (op (not Boolean) Boolean)
  (=> (not true) false)
  (=> (not false) true)
  (=> #:vars ([X Boolean]) (not (not X)) X)

  (op (and Boolean ...) Boolean #:symmetric)
  (=> #:vars ([X Boolean]) (and X) X)
  (=> #:vars ([Xs Boolean ...]) (and false Xs) false)
  (=> #:vars ([Xs Boolean ...]) (and true  Xs) (and Xs))
  (=> #:vars ([X Boolean] [Xs Boolean ?...]) (and X X Xs) (and X Xs))

  (op (or Boolean ...) Boolean #:symmetric)
  (=> #:vars ([X Boolean]) (or X) X)
  (=> #:vars ([Xs Boolean ...]) (or true  Xs) true)
  (=> #:vars ([Xs Boolean ...]) (or false Xs) (or Xs))
  (=> #:vars ([X Boolean] [Xs Boolean ?...]) (or X X Xs) (or X Xs))
  
  (op (xor Boolean ...) Boolean #:symmetric)
  (=> #:vars ([X Boolean]) (xor X) X)
  (=> #:vars ([Xs Boolean ...]) (xor false Xs) (xor Xs))
  (=> #:vars ([Xs Boolean ...]) (xor true  Xs) (not (xor Xs)))
  (=> #:vars ([X Boolean]) (xor X X) false)
  (=> #:vars ([X Boolean] [Xs Boolean ...]) (xor X X Xs) (xor Xs)))
