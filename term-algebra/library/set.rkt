#lang racket

(provide set)

(require term-algebra/basic-api)

(define-node set

  (use builtin:equality)

  (sorts Element Set NonEmptySet)
  (subsorts [NonEmptySet Set])

  (op (set Element ...) NonEmptySet #:symmetric)
  (op (set) Set)
  (=> #:vars ([E Element] [Es Element ?...])
      (set E E Es)
      (set E Es))

  (op (cons Element Set) NonEmptySet)
  (=> #:vars ([E Element] [Es Element ?...])
      (cons E (set Es))
      (set E Es))

  (op (contains? Element Set) Boolean)
  (=> #:vars ([E Element])
      (contains? E (set E))
      true)
  (=> #:vars ([E Element] [Es Element ?...])
      (contains? E (set E Es))
      true)
  (=> #:vars ([E Element] [S Set])
      (contains? E S)
      false))

