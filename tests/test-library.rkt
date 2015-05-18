#lang racket

(provide library-tests)

(require rackunit
         term-algebra/basic-api
         term-algebra/library/boolean
         term-algebra/library/list
         term-algebra/library/set
         term-algebra/library/node-transforms
         (prefix-in meta: term-algebra/meta))

(define-syntax-rule (check-reduce node initial-term reduced-term)
  (check meta:vterm-equal?
         (reduce (term node initial-term))
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
                  (add (use (builtin-node 'symbol)))
                  (rename-sort 'Element 'Symbol)
                  (rename-sort 'List 'SymbolList)
                  (rename-sort 'NonEmptyList 'NESymbolList)
                  (rename-op 'list 'symbols))))))
    
    (check-reduce symbol-list (cons 'X (symbols 'A 'B)) (symbols 'X 'A 'B))
    (check-reduce symbol-list (cons 'X (symbols)) (symbols 'X))
    (check-reduce symbol-list (append 'X (symbols 'A 'B)) (symbols 'A 'B 'X))
    (check-reduce symbol-list (append 'X (symbols)) (symbols 'X))
    (check-reduce symbol-list (head (symbols 'A 'B)) 'A)
    (check-reduce symbol-list (tail (symbols 'A 'B)) (symbols 'B))
    (check-reduce symbol-list (contains? 'A (symbols 'A 'B)) true)
    (check-reduce symbol-list (contains? 'X (symbols 'A 'B)) false))

  (test-case "set transforms"

    (define symbol-set
      (reduce
       (term node-transforms
             (transformed-node ,set
                 (transforms
                  (node-name 'set-of-symbols)
                  (add (use (builtin-node 'symbol)))
                  (rename-sort 'Element 'Symbol)
                  (rename-sort 'Set 'SymbolSet)
                  (rename-sort 'NonEmptySet 'NESymbolSet)
                  (rename-op 'set 'symbols))))))

    (check-reduce symbol-set (symbols 'A 'A 'B 'C 'B 'A) (symbols 'A 'B 'C))
    (check-reduce symbol-set (cons 'X (symbols 'A 'A 'B 'C 'B 'A))
                             (symbols 'A 'B 'C 'X))
    (check-reduce symbol-set (cons 'X (symbols))
                             (symbols 'X))
    (check-reduce symbol-set (contains? 'A (symbols 'A 'B 'C 'B)) true)
    (check-reduce symbol-set (contains? 'X (symbols 'A 'B 'C 'B)) false))

  (test-case "node construction"

    (define-node test-node
      (sorts A B C)
      (subsorts [A B] [A C])
      (op foo A)
      (op (bar A) B)
      (=> (bar foo) foo))
    
    (define-node empty)
    (define constructed-test-node
      (reduce
       (term node-transforms
             (transformed-node ,empty
                 (transforms
                  (node-name 'test-node)
                  (add (sort 'A))
                  (add (sorts 'B 'C))
                  (add (subsort 'A 'B))
                  (add (subsorts (subsort 'A 'C)))
                  (add (op 'foo (domain) 'A))
                  (add (op 'bar (domain 'A) 'B))
                  (add (=> (vars)
                           (pattern 'bar (args (pattern 'foo (args))))
                           (pattern 'foo (args))
                           no-condition)))))))

    (define constructed-test-node-2
      (reduce
       (term flex-node
             (node 'test-node
                   (declarations
                    (sort 'A)
                    (sorts 'B 'C)
                    (subsort 'A 'B)
                    (subsorts (subsort 'A 'C))
                    (op 'foo (domain) 'A)
                    (op 'bar (domain 'A) 'B)
                    (=> (vars)
                        (pattern 'bar (args (pattern 'foo (args))))
                        (pattern 'foo (args))
                        no-condition))))))

    (check meta:vterm-equal?
           (meta:meta-down node-transforms (meta:meta-up test-node))
           constructed-test-node)
    (check meta:vterm-equal?
           (meta:meta-down flex-node (meta:meta-up test-node))
           constructed-test-node-2)
    (check-reduce test-node (bar foo) foo)
    (check-reduce constructed-test-node (bar foo) foo)
    (check-reduce constructed-test-node-2 (bar foo) foo)))

(module* main #f
  (require rackunit/text-ui)
  (run-tests library-tests))
