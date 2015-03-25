#lang racket

(provide module-tests)

(require rackunit
         (prefix-in operators: term-algebra/operators)
         (prefix-in modules: term-algebra/modules)
         (prefix-in api: term-algebra/basic-api)
         term-algebra/library/boolean)

(require/expose term-algebra/operators [op-set-ops
                                        operator-signatures])
(require/expose term-algebra/meta [internal-module])

(api:define-module test
  (use boolean)
  (include boolean))

(define-test-suite module-tests

  (test-case "op-verification"
    (define ops1 (op-set-ops
                  (modules:module-ops (internal-module boolean))))
    (define ops2 (op-set-ops (modules:module-ops (internal-module test))))
    (define sig-1 (list (set 'Boolean)))
    (define sig-n (set 'Boolean))
    (check-equal? (hash-count ops1) 6)
    (check-equal? (hash-count ops2) 6)
    (check-equal? (hash-count (hash-ref ops1 'not )) 1)
    (check-equal? (hash-count (hash-ref ops2 'not )) 1)
    (check-equal? (length (operator-signatures
                           (hash-ref (hash-ref ops1 'not ) sig-1))) 1)
    (check-equal? (length (operator-signatures
                           (hash-ref (hash-ref ops2 'not ) sig-1))) 1)
    (check-equal? (length (operator-signatures
                           (hash-ref (hash-ref ops1 'and ) sig-n))) 1)
    (check-equal? (length (operator-signatures
                           (hash-ref (hash-ref ops2 'and ) sig-n))) 1))

  (test-case "rule-verification"
    (define boolean-rules (modules:module-rules
                           (internal-module boolean)))
    (define test-rules (modules:module-rules (internal-module test)))
    (check-equal? (length (hash-ref boolean-rules 'not))
                  2)
    (check-equal? (length (hash-ref boolean-rules 'and))
                  3)
    (check-equal? (length (hash-ref boolean-rules 'or))
                  3)
    (check-equal? (length (hash-ref boolean-rules 'xor))
                  3)
    (check-equal? (length (hash-ref test-rules 'not))
                  2)
    (check-equal? (length (hash-ref test-rules 'and))
                  3)
    (check-equal? (length (hash-ref test-rules 'or))
                  3)
    (check-equal? (length (hash-ref test-rules 'xor))
                  3)))

(module* main #f
  (require rackunit/text-ui)
  (run-tests module-tests))
