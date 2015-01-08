#lang racket

(provide operator-tests)

(require rackunit
         (only-in term-algebra/syntax define-module term)
         (only-in term-algebra/modules sort-from module-sorts)
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in builtin: term-algebra/builtin))

(require/expose term-algebra/operators
                [operator-signatures op-set-ops])

(define-module test
  (sorts A B C X Y Z)
  (subsorts [A C] [B C] [X Y] [X Z]))

(define test-sorts (module-sorts test))
(define A-kind (sorts:kind 'A test-sorts))
(define X-kind (sorts:kind 'X test-sorts))

(define test-ops
  (let* ([ops (operators:empty-op-set test-sorts)]
         [ops (operators:add-op
               'foo (list 'A) 'X (set)
               ops)]
         [ops (operators:add-op
               'foo (list 'C) 'Z (set)
               ops)])
    ops))

(define-test-suite operator-tests
  
  (test-case "test-sorts"
    (check-equal? A-kind (set 'A 'B 'C))
    (check-equal? X-kind (set 'X 'Y 'Z)))
  
  (test-case "operator-definition"
    (check-equal? (hash-count
                   (hash-ref
                    (op-set-ops (operators:add-op
                                 'foo (list 'Z) 'A (set)
                                 test-ops))
                    'foo))
                  2)
    (check-equal? (hash-count
                   (hash-ref
                    (op-set-ops (operators:add-op
                                 'foo (list 'B) 'Z (set)
                                 test-ops))
                    'foo))
                  1)
    (check-equal? (length
                   (operator-signatures
                    (hash-ref (hash-ref
                               (op-set-ops (operators:add-op
                                            'foo (list 'B) 'Z (set)
                                            test-ops))
                               'foo)
                              (list A-kind))))
                  3))

  (test-case "operator-lookup"
    (check-equal? (operators:lookup-op 'foo (list 'A) test-ops)
                  'X)
    (check-equal? (operators:lookup-op 'foo (list 'B) test-ops)
                  'Z)
    (check-equal? (operators:lookup-op 'foo (list 'C) test-ops)
                  'Z)
    (check-equal? (operators:lookup-op 'foo (list 'X) test-ops)
                  #f))
  
  (test-exn "preregularity"
      #rx"Operator .*bar.* is not preregular.*"
      (lambda ()
        (operators:add-op
         'bar (list 'C 'B) 'Z (set)
         (operators:add-op
          'bar (list 'A 'C) 'Y (set)
          test-ops))))

  (test-exn "wrong-range-kind"
      #rx"Operator .*foo.* must have the kind of sort.*"
      (lambda () 
        (operators:add-op
         'foo (list 'B) 'A (set)
         test-ops)))

  (test-exn "mixed-var-args"
      #rx"Operator .*foo.* must have properties .*"
      (lambda () 
        (operators:add-op
         'foo (list 'B) 'Z (set 'variable-length-domain)
         test-ops))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests operator-tests))
