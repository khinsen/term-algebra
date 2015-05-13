#lang racket

(provide list)

(require term-algebra/basic-api)

(define-node list

  (use builtin:equality)
  
  (sorts Element List NonEmptyList)
  (subsorts [NonEmptyList List])
  
  (op (list Element ...) NonEmptyList)
  (op (list) List)

  (op (head NonEmptyList) Element)
  (=> #:vars ([E Element] [Es Element ?...])
      (head (list E Es))
      E)
  (op (tail NonEmptyList) List)
  (=> #:vars ([E Element] [Es Element ?...])
      (tail (list E Es))
      (list Es))
  
  (op (cons Element List) NonEmptyList)
  (=> #:vars ([E Element] [Es Element ?...])
      (cons E (list Es))
      (list E Es))

  (op (append Element List) NonEmptyList)
  (=> #:vars ([NE Element])
      (append NE (list))
      (list NE))
  (=> #:vars ([NE Element] [E Element] [Es Element ?...])
      (append NE (list E Es))
      (cons E (append NE (list Es))))
  
  (op (contains? Element List) Boolean)
  (=> #:vars ([E Element])
      (contains? E (list))
      false)
  (=> #:vars ([E Element] [Es Element ?...])
      (contains? E (list E Es))
      true)
  (=> #:vars ([E Element] [E1 Element] [Es Element ?...])
      (contains? E (list E1 Es))
      (contains? E (list Es))))

