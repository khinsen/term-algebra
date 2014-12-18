#lang racket

(provide builtin-tests)

(require rackunit
         (only-in term-algebra/rewrite reduce)
         (only-in term-algebra/modules term define-module)
         term-algebra/builtin)

(define-syntax-rule (test-reduce module initial-term reduced-term)
  (test-equal? (symbol->string (quote module))
               (reduce (term module initial-term))
               (term module reduced-term)))

(define-test-suite builtin-tests

  (test-reduce equality (== false false)
                        true)
  (test-reduce equality (== true true)
                        true)
  (test-reduce equality (== false true)
                        false)
  (test-reduce equality (== true false)
                        false)
  (test-reduce equality (== (== true true) (== false false))
                        true)

  (test-reduce exact-number (+ 2 3)
                            5)
  (test-reduce exact-number (+ 2/3 1/3)
                            1)
  (test-reduce exact-number (- 2 3)
                            -1)
  (test-reduce exact-number (* 2 3)
                            6)
  (test-reduce exact-number (/ 2 3)
                            2/3)
  (test-reduce exact-number (/ 1 0)
                            (/ 1 0))
  (test-reduce exact-number (div 2 3)
                            0)
  (test-reduce exact-number (> 2 3)
                            false)
  (test-reduce exact-number (< 2 3)
                            true)
  (test-reduce exact-number (= 2 3)
                            false)
  (test-reduce exact-number (= 1/5 2/10)
                            true)

  (test-exn "too-many-vars"
            #rx"vars unused in left-hand-side.*"
            (lambda () (define-module test
                    (use truth)
                    (=-> #:var X true false))
               test))

  (test-exn "no-rule-for-imported-op"
            #rx"cannot add rule to imported operator.*"
            (lambda () (define-module test
                    (use truth)
                    (=-> true false))
               test))
  (test-not-exn "rule-for-imported-op"
            (lambda () (define-module test
                    (extend truth)
                    (=-> true false))
               test))

  (test-not-exn "string-imported"
            (lambda () (define-module test
                    (use string)
                    (op foo String)
                    (=-> foo "foo"))
               test))
  (test-exn "string-not-imported"
            #rx"import the string module.*"
            (lambda () (define-module test
                    (op foo String)
                    (=-> foo "foo"))
               test))

  (test-not-exn "symbol-imported"
            (lambda () (define-module test
                    (use symbol)
                    (op foo Symbol)
                    (=-> foo 'foo))
               test))
  (test-exn "symbol-not-imported"
            #rx"import the symbol module.*"
            (lambda () (define-module test
                    (op foo Symbol)
                    (=-> foo 'foo))
               test))
  
  (test-not-exn "number-imported"
            (lambda () (define-module test
                    (use exact-number)
                    (op foo ExactNumber)
                    (=-> foo 2))
               test))
  (test-exn "number-not-imported"
            #rx"import the exact-number module.*"
            (lambda () (define-module test
                    (op foo ExactNumber)
                    (=-> foo 2))
               test)))


(module* main #f
  (require rackunit/text-ui)
  (run-tests builtin-tests))
