#lang racket

(provide boolean-tests)

(require rackunit
         term-algebra/modules
         term-algebra/rewrite
         (only-in term-algebra/builtin truth))

(define-syntax-rule (test-reduce module initial-term reduced-term)
  (test-equal? (symbol->string (quote module))
               (reduce (term module initial-term))
               (term module reduced-term)))

(define-module boolean

  (use truth)
 
  (op (not x))
  (=-> (not true) false)
  (=-> (not false) true)

  (op (and x y))
  (=-> (and true true) true)
  (=-> #:var X (and false X) false)
  (=-> #:var X (and X false) false)

  (op (or x y))
  (=-> (or false false) false)
  (=-> #:var X (or true X) true)
  (=-> #:var X (or X true) true))

(define-test-suite boolean-tests

  (test-reduce boolean (not true)
                       false)
  (test-reduce boolean (not false)
                       true)

  (test-reduce boolean (and true true)
                       true)
  (test-reduce boolean (and false false)
                       false)
  (test-reduce boolean (and true false)
                       false)
  (test-reduce boolean (and false true)
                       false)
  (test-reduce boolean (and (not false) (not false))
                       true)

  (test-reduce boolean (or true true)
                       true)
  (test-reduce boolean (or false false)
                       false)
  (test-reduce boolean (or true false)
                       true)
  (test-reduce boolean (or false true)
                       true)
  (test-reduce boolean (or (not false) (not false))
                       true)
  
  (test-case "term-syntax"

             (check-exn #rx"wrong number of arguments.*"
                        (lambda () (term boolean (false false))))

             (check-exn #rx"undefined op or var.*"
                        (lambda () (term boolean foo)))
             (check-exn #rx"undefined op or var.*"
                        (lambda () (term boolean (foo false))))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests boolean-tests))
