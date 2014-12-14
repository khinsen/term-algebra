#lang racket

(require rackunit
         term-algebra/modules
         term-algebra/rewrite
         (only-in term-algebra/builtin truth))

(define-syntax-rule (check-reduce module initial-term reduced-term)
  (check-equal? (reduce (term module initial-term)) (term module reduced-term)))

(define-module boolean

  (use truth)
 
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

  (check-reduce boolean (not true)
                        false)
  (check-reduce boolean (not false)
                        true)

  (check-reduce boolean (and true true)
                        true)
  (check-reduce boolean (and false false)
                        false)
  (check-reduce boolean (and true false)
                        false)
  (check-reduce boolean (and false true)
                        false)
  (check-reduce boolean (and (not false) (not false))
                        true)

  (check-reduce boolean (or true true)
                        true)
  (check-reduce boolean (or false false)
                        false)
  (check-reduce boolean (or true false)
                        true)
  (check-reduce boolean (or false true)
                        true)
  (check-reduce boolean (or (not false) (not false))
                        true))


(test-case "term-syntax"

  (check-exn #rx"wrong number of arguments.*"
             (lambda () (term boolean (false false))))

  (check-exn #rx"undefined op or var.*"
             (lambda () (term boolean foo)))
  (check-exn #rx"undefined op or var.*"
             (lambda () (term boolean (foo false)))))
