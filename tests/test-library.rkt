#lang racket

(provide library-tests)

(require rackunit
         term-algebra/api
         term-algebra/library/boolean
         term-algebra/library/list
         term-algebra/library/module-transforms)

(define-syntax-rule (check-reduce module initial-term reduced-term)
  (check-equal? (reduce (term module initial-term))
                (term module reduced-term)))

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
    (check-reduce boolean (xor (not false) (not true)) true))

  (test-case "list transforms"

    (define symbol-list
      (reduce
       (term module-transforms
             (transformed-module ,list
                 (transforms
                  (module-name 'list-of-symbols)
                  (add-import (use (builtin-module 'symbol)))
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
