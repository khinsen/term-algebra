#lang racket

(require rackunit
         term-algebra/client
         (only-in term-algebra/terms reduce))

(define (check-reduce term reduced-term)
  (check-equal? (reduce term) reduced-term))

(use-module boolean "./boolean.rkt")

(test-case "boolean"

  (check-reduce (term boolean (not true))
                (term boolean false))
  (check-reduce (term boolean (not false))
                (term boolean true))

  (check-reduce (term boolean (and true true))
                (term boolean true))
  (check-reduce (term boolean (and false false))
                (term boolean false))
  (check-reduce (term boolean (and true false))
                (term boolean false))
  (check-reduce (term boolean (and false true))
                (term boolean false))
  (check-reduce (term boolean (and (not false) (not false)))
                (term boolean true))
   
  (check-reduce (term boolean (or true true))
                (term boolean true))
  (check-reduce (term boolean (or false false))
                (term boolean false))
  (check-reduce (term boolean (or true false))
                (term boolean true))
  (check-reduce (term boolean (or false true))
                (term boolean true))
  (check-reduce (term boolean (or (not false) (not false)))
                (term boolean true)))


(test-case "term-syntax"

  (check-exn #rx"illegal syntax.*"
             (lambda () (term boolean 42)))
  (check-exn #rx"illegal syntax.*"
             (lambda () (term boolean (false 42))))
  (check-exn #rx"illegal syntax.*"
             (lambda () (term boolean (42 false))))

  (check-exn #rx"wrong number of arguments.*"
             (lambda () (term boolean (false false))))

  (check-exn #rx"unbound symbol.*"
             (lambda () (term boolean foo)))
  (check-exn #rx"unbound symbol.*"
             (lambda () (term boolean (foo false)))))
