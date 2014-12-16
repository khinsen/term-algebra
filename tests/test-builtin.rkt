#lang racket

(provide builtin-tests)

(require rackunit
         (only-in term-algebra/rewrite reduce)
         (only-in term-algebra/modules term define-module)
         (only-in term-algebra/builtin truth equality string symbol))

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
                    (op foo)
                    (=-> foo "foo"))
               test))
  (test-exn "string-not-imported"
            #rx"import the string module.*"
            (lambda () (define-module test
                    (op foo)
                    (=-> foo "foo"))
               test))

  (test-not-exn "symbol-imported"
            (lambda () (define-module test
                    (use symbol)
                    (op foo)
                    (=-> foo 'foo))
               test))
  (test-exn "symbol-not-imported"
            #rx"import the symbol module.*"
            (lambda () (define-module test
                    (op foo)
                    (=-> foo 'foo))
               test)))


(module* main #f
  (require rackunit/text-ui)
  (run-tests builtin-tests))
