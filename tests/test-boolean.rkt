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

(define-syntax-rule (test-reduce-all initial-term reduced-term)
  (begin
    (test-reduce boolean initial-term reduced-term)
    (test-reduce use-boolean initial-term reduced-term)
    (test-reduce extend-boolean initial-term reduced-term)))

(define-module boolean

  (use truth)
 
  (op (not Boolean) Boolean)
  (=-> (not true) false)
  (=-> (not false) true)

  (op (and Boolean Boolean) Boolean)
  (=-> (and true true) true)
  (=-> #:var X (and false X) false)
  (=-> #:var X (and X false) false)

  (op (or Boolean Boolean) Boolean)
  (=-> (or false false) false)
  (=-> #:var X (or true X) true)
  (=-> #:var X (or X true) true))

(define-module use-boolean
  (use boolean))

(define-module extend-boolean
  (extend boolean))

(define-test-suite boolean-tests

  (test-reduce-all (not true)
                   false)
  (test-reduce-all (not false)
                   true)

  (test-reduce-all (and true true)
                   true)
  (test-reduce-all (and false false)
                   false)
  (test-reduce-all (and true false)
                   false)
  (test-reduce-all (and false true)
                   false)
  (test-reduce-all (and (not false) (not false))
                   true)

  (test-reduce-all (or true true)
                   true)
  (test-reduce-all (or false false)
                   false)
  (test-reduce-all (or true false)
                   true)
  (test-reduce-all (or false true)
                   true)
  (test-reduce-all (or (not false) (not false))
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
