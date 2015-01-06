#lang racket

(provide operator-tests)

(require rackunit
         (only-in term-algebra/syntax define-module term)
         (only-in term-algebra/modules sort-from module-sorts)
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in builtin: term-algebra/builtin))

(define-module test
  (sorts A B C X Y Z)
  (subsorts [A C] [B C] [X Y] [Y Z]))

(define test-sorts (module-sorts test))

(define test-ops
  (let* ([ops (operators:empty-op-set)]
         [ops (operators:add-op
               (operators:op 'foo (list 'A) 'X (set))
               test-sorts ops)]
         [ops (operators:add-op
               (operators:op 'foo (list 'C) 'Z (set))
               test-sorts ops)])
    ops))

(define-test-suite operator-tests
  
  (test-case "operator-definition"

    (check-equal? (hash-count (hash-ref (operators:add-op
                                         (operators:op 'foo (list 'Z) 'A (set))
                                         test-sorts test-ops)
                                        'foo))
                  2)
    (check-equal? (hash-count (hash-ref (operators:add-op
                                         (operators:op 'foo (list 'B) 'Z (set))
                                         test-sorts test-ops)
                                        'foo))
                  1))
  
  (test-exn "wrong-range-kind"
      #rx"Operator .*foo.* must have the kind of sort.*"
    (lambda () 
      (operators:add-op
       (operators:op 'foo (list 'B) 'A (set))
       test-sorts test-ops)))

  (test-exn "mixed-var-args"
      #rx"Operator .*foo.* must have properties .*"
    (lambda () 
      (operators:add-op
       (operators:op 'foo (list 'B) 'Z (set 'variable-length-domain))
       test-sorts test-ops))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests operator-tests))
