#lang racket

(provide node-tests)

(require rackunit
         (prefix-in operators: term-algebra/operators)
         (prefix-in nodes: term-algebra/nodes)
         (prefix-in api: term-algebra/basic-api)
         term-algebra/library/boolean)

(require/expose term-algebra/operators [op-set-ops
                                        operator-signatures])
(require/expose term-algebra/meta [internal-node])

(api:define-node test
  (use boolean)
  (include boolean))

(define-test-suite node-tests

  (test-case "op-verification"
    (define ops1 (op-set-ops
                  (nodes:node-ops (internal-node boolean))))
    (define ops2 (op-set-ops (nodes:node-ops (internal-node test))))
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
    (define boolean-rules (nodes:node-rules
                           (internal-node boolean)))
    (define test-rules (nodes:node-rules (internal-node test)))
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
  (run-tests node-tests))
