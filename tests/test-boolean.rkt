#lang racket

(require rackunit
         term-algebra/modules
         (only-in term-algebra/terms reduce))

(define (check-reduce term reduced-term)
  (check-equal? (reduce term) reduced-term))

(define-module boolean

  (define-op true)
  (define-op false)
 
  (define-op (not x))
  (=-> (not true) false)
  (=-> (not false) true)

  (define-op (and x y))
  (=-> (and true true) true)
  (=-> #:var X (and false X) false)
  (=-> #:var X (and X false) false)

  (define-op (or x y))
  (=-> (or false false) false)
  (=-> #:var X (or true X) true)
  (=-> #:var X (or X true) true))

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

  (check-exn #rx"wrong number of arguments.*"
             (lambda () (term boolean (false false))))

  (check-exn #rx"undefined op or var.*"
             (lambda () (term boolean foo)))
  (check-exn #rx"undefined op or var.*"
             (lambda () (term boolean (foo false)))))
