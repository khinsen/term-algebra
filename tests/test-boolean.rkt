#lang racket

(require rackunit
         racket/splicing
         (only-in term-algebra/main term)
         (only-in term-algebra/terms reduce))

(define (check-reduce term reduced-term)
  (check-equal? (reduce term) reduced-term))

(test-case "boolean"

  (local-require "./boolean.rkt")
  
  (check-reduce (term (not true))
                (term false))
  (check-reduce (term (not false))
                (term true))

  (check-reduce (term (and true true))
                (term true))
  (check-reduce (term (and false false))
                (term false))
  (check-reduce (term (and true false))
                (term false))
  (check-reduce (term (and false true))
                (term false))
  (check-reduce (term (and (not false) (not false)))
                (term true))
   
  (check-reduce (term (or true true))
                (term true))
  (check-reduce (term (or false false))
                (term false))
  (check-reduce (term (or true false))
                (term true))
  (check-reduce (term (or false true))
                (term true))
  (check-reduce (term (or (not false) (not false)))
                (term true)))

