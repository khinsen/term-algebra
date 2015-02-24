#lang racket

(provide module-tests)

(require rackunit
         (prefix-in operators: term-algebra/operators)
         (prefix-in modules: term-algebra/modules)
         (prefix-in api: term-algebra/api)
         (prefix-in library: term-algebra/library))

(require/expose term-algebra/operators [op-set-ops
                                        operator-signatures])

(api:define-module test
  (use library:boolean)
  (extend library:boolean))

(define-test-suite module-tests

  (test-case "op-verification"
    (define ops1 (op-set-ops (modules:module-ops library:boolean)))
    (define ops2 (op-set-ops (modules:module-ops test)))
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
    (check-equal? (length (hash-ref (modules:module-rules library:boolean) 'not))
                  2)
    (check-equal? (length (hash-ref (modules:module-rules library:boolean) 'and))
                  3)
    (check-equal? (length (hash-ref (modules:module-rules library:boolean) 'or))
                  3)
    (check-equal? (length (hash-ref (modules:module-rules library:boolean) 'xor))
                  3)
    (check-equal? (length (hash-ref (modules:module-rules test) 'not))
                  2)
    (check-equal? (length (hash-ref (modules:module-rules test) 'and))
                  3)
    (check-equal? (length (hash-ref (modules:module-rules test) 'or))
                  3)
    (check-equal? (length (hash-ref (modules:module-rules test) 'xor))
                  3)))

(module* main #f
  (require rackunit/text-ui)
  (run-tests module-tests))
