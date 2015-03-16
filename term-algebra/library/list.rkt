#lang racket

(provide list)

(require term-algebra/api)

(define-module list

  (use builtin:equality)
  
  (sorts Element List NonEmptyList)
  (subsorts [NonEmptyList List])
  
  (op (list Element ...) NonEmptyList)
  (op (list) List)

  (op (head NonEmptyList) Element)
  (=-> #:vars ([E Element] [ES Element ...])
       (head (list E ES))
       E)
  (op (tail NonEmptyList) List)
  (=-> #:vars ([E Element] [ES Element ...])
       (tail (list E ES))
       (list ES))
  
  (op (cons Element List) NonEmptyList)
  (=-> #:vars ([E Element] [ES Element ...])
       (cons E (list ES))
       (list E ES))
  
  (op (contains? Element List) Boolean)
  (=-> #:vars ([E Element])
       (contains? E (list))
       false)
  (=-> #:vars ([E Element] [Es Element ...])
       (contains? E (list E Es))
       true)
  (=-> #:vars ([E Element] [E1 Element] [Es Element ...])
       (contains? E (list E1 Es))
       (contains? E (list Es))))

