#lang racket

(provide operator-tests)

(require rackunit
         (only-in term-algebra/modules define-builtin-module
                                       sort-from module-sorts module-ops)
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators))

(require/expose term-algebra/operators
                [operator-signatures op-set-ops])

(define-builtin-module test
  (sorts A B C X Y Z)
  (subsorts [A C] [B C] [X Y] [X Z])
  (op (foo A) X)
  (op (foo C) Z))

(define test-sorts (module-sorts test))
(define test-ops (module-ops test))
(define A-kind (sorts:kind 'A test-sorts))
(define X-kind (sorts:kind 'X test-sorts))

(define-test-suite operator-tests
  
  (test-case "test-sorts"
    (check-equal? A-kind (set 'A 'B 'C))
    (check-equal? X-kind (set 'X 'Y 'Z)))
  
  (test-case "operator-lookup"
    (check-equal? (operators:lookup-op 'foo (list 'A) test-ops)
                  'X)
    (check-equal? (operators:lookup-op 'foo (list 'B) test-ops)
                  'Z)
    (check-equal? (operators:lookup-op 'foo (list 'C) test-ops)
                  'Z)
    (check-equal? (operators:lookup-op 'foo (list 'X) test-ops)
                  #f))
  
  (test-case "operator-definition"
    
    (let ()
      (define-builtin-module test2
        (extend test)
        (op (foo Z) A))
      (let* ([op-hash (op-set-ops (module-ops test2))]
             [foo-op (hash-ref op-hash 'foo)])
        (check-equal? 
         (hash-count foo-op) ; Number of different kinds
         2)
        (check-equal?
         (length (operator-signatures (hash-ref foo-op (list A-kind))))
         2)
        (check-equal?
         (length (operator-signatures (hash-ref foo-op (list X-kind))))
         1)))

    (let ()
      (define-builtin-module test2
        (extend test)
        (op (foo B) Z))
      (let* ([op-hash (op-set-ops (module-ops test2))]
             [foo-op (hash-ref op-hash 'foo)])
        (check-equal? 
         (hash-count foo-op)  ; Number of different kinds
         1)
        (check-equal?
         (length (operator-signatures (hash-ref foo-op (list A-kind))))
         3))))

  (test-case "variable-arity"
    (let ()
      (define-builtin-module test2
        (extend test)
        (op (foo Z ...) A))
      (let* ([op-hash (op-set-ops (module-ops test2))]
             [foo-op (hash-ref op-hash 'foo)])
        (check-equal? 
         (hash-count foo-op) ; Number of different signatures
         2)
        (check-equal?
         (length (operator-signatures (hash-ref foo-op (list A-kind))))
         2)
        (check-equal?
         (length (operator-signatures (hash-ref foo-op X-kind)))
         1))))

  (test-exn "preregularity"
      #rx"Operator .*bar.* is not preregular.*"
    (lambda ()
      (define-builtin-module error
        (extend test)
        (op (bar C B) Z)
        (op (bar A C) Y))
      (void)))

  (test-exn "wrong-range-kind"
      #rx"Operator .*foo.* must have the kind of sort.*"
    (lambda ()
      (define-builtin-module error
        (extend test)
        (op (foo B) A))
      (void)))

  (test-exn "mixed-var-args"
      #rx"Conflicting fixed and variable arity definitions.*"
      (lambda () 
        (define-builtin-module error
          (extend test)
          (op (foo B ...) Z))
        (void))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests operator-tests))
