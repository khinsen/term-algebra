#lang racket

(provide library-tests)

(require rackunit
         term-algebra/basic-api
         term-algebra/library/boolean
         term-algebra/library/list
         term-algebra/library/node-transforms)

(define-syntax-rule (check-reduce node initial-term reduced-term)
  (check-equal? (reduce (term node initial-term))
                (term node reduced-term)))

(define-node boolean2
  (use boolean)
  (op foo Boolean))

(define-test-suite library-tests

  (test-case "boolean"
    (check-reduce boolean (not true)  false)
    (check-reduce boolean (not false) true)

    (check-reduce boolean (and true true)   true)
    (check-reduce boolean (and false false) false)
    (check-reduce boolean (and true false)  false)
    (check-reduce boolean (and false true)  false)
    (check-reduce boolean (and false true false true) false)
    (check-reduce boolean (and (not false) (not false)) true)

    (check-reduce boolean (or true true)   true)
    (check-reduce boolean (or false false) false)
    (check-reduce boolean (or true false)  true)
    (check-reduce boolean (or false true)  true)
    (check-reduce boolean (or false true false true) true)
    (check-reduce boolean (or (not false) (not false)) true)

    (check-reduce boolean (xor true true)   false)
    (check-reduce boolean (xor false false) false)
    (check-reduce boolean (xor true false)  true)
    (check-reduce boolean (xor false true)  true)
    (check-reduce boolean (xor false true false true) false)
    (check-reduce boolean (xor (not false) (not true)) true)

    (check-reduce boolean2 (and foo foo false foo) false)
    (check-reduce boolean2 (and foo foo true foo)  foo)
    (check-reduce boolean2 (or foo foo true foo)   true)
    (check-reduce boolean2 (or foo foo false foo)  foo)
    (check-reduce boolean2 (xor foo false true)    (not foo))
    (check-reduce boolean2 (xor foo foo foo)       foo))

  (test-case "list transforms"

    (define symbol-list
      (reduce
       (term node-transforms
             (transformed-node ,list
                 (transforms
                  (node-name 'list-of-symbols)
                  (add-import (use (builtin-node 'symbol)))
                  (rename-sort 'Element 'Symbol)
                  (rename-sort 'List 'SymbolList)
                  (rename-sort 'NonEmptyList 'NESymbolList)
                  (rename-op 'list 'symbols))))))
    
    (check-reduce symbol-list (cons 'X (symbols 'A 'B)) (symbols 'X 'A 'B))
    (check-reduce symbol-list (head (symbols 'A 'B)) 'A)
    (check-reduce symbol-list (tail (symbols 'A 'B)) (symbols 'B))
    (check-reduce symbol-list (contains? 'A (symbols 'A 'B)) true)
    (check-reduce symbol-list (contains? 'X (symbols 'A 'B)) false)))

(module* main #f
  (require rackunit/text-ui)
  (run-tests library-tests))
