#lang racket

(provide module-tests)

(require rackunit
         (only-in term-algebra/syntax define-module term)
         (only-in term-algebra/modules sort-from module-sorts)
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in builtin: term-algebra/builtin))

(define-test-suite module-tests
  
  (test-exn "undefined-range-sort"
            #rx"undefined sort.*"
            (lambda () (define-module test
                    (op foo a))
               test))
  (test-exn "undefined-arg-sort"
            #rx"undefined sort.*"
            (lambda () (define-module test
                    (sorts a)
                    (op (foo b) a))
               test))

  (test-exn "wrong-number-of-fixed-args"
            #rx"wrong number of arguments.*"
            (lambda () (define-module test
                    (use builtin:truth)
                    (op (foo Boolean) Boolean))
               (term test foo)))
  (test-exn "wrong-number-of-fixed-args"
            #rx"wrong number of arguments.*"
            (lambda () (define-module test
                    (use builtin:truth)
                    (op (foo Boolean) Boolean))
               (term test (foo true false))))
  (test-exn "wrong-number-of-variable-args"
            #rx"too few arguments.*"
            (lambda () (define-module test
                    (use builtin:truth)
                    (op (foo Boolean Boolean ...) Boolean))
               (term test (foo)))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests module-tests))
