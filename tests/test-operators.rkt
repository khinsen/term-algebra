#lang racket

(provide operator-tests)

(require rackunit
         (only-in term-algebra/nodes define-builtin-node node-ops)
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators))

(require/expose term-algebra/operators
                [operator-signatures op-set-ops])

(define (node-sorts node)
  (operators:op-set-sorts (node-ops node)))

(define-builtin-node test-op
  (sorts A B C X Y Z)
  (subsorts [A C] [B C] [X Y] [X Z])
  (op (foo A) X)
  (op (foo C) Z))

(define test-sorts (node-sorts test-op))
(define test-ops (node-ops test-op))
(define A-kind (sorts:kind 'A test-sorts))
(define X-kind (sorts:kind 'X test-sorts))

(define-builtin-node test-op-any
  (sorts Any X Y)
  (op (foo Any) X)
  (op (foo X Any) Y)
  (op (bar Any ...) X))

(define test-any-ops (node-ops test-op-any))

(define-test-suite operator-tests

  (test-case "test-sorts"
    (check-equal? A-kind (set 'A 'B 'C))
    (check-equal? X-kind (set 'X 'Y 'Z)))

  (test-case "operator-lookup"
    (check-equal? (operators:lookup-range 'foo (list 'A) test-ops)
                  'X)
    (check-equal? (operators:lookup-range 'foo (list 'B) test-ops)
                  'Z)
    (check-equal? (operators:lookup-range 'foo (list 'C) test-ops)
                  'Z)
    (check-equal? (operators:lookup-range 'foo (list 'X) test-ops)
                  #f))

  (test-case "operator-lookup-any"
    (check-equal? (operators:lookup-range 'foo (list 'X) test-any-ops)
                  'X)
    (check-equal? (operators:lookup-range 'foo (list 'X 'X) test-any-ops)
                  'Y)
    (check-equal? (operators:lookup-range 'bar empty test-any-ops)
                  #f)
    (check-equal? (operators:lookup-range 'bar (list 'X) test-any-ops)
                  'X)
    (check-equal? (operators:lookup-range 'bar (list 'X 'X) test-any-ops)
                  'X))

  (test-case "operator-definition"

    (let ()
      (define-builtin-node test-op-1
        (include test-op)
        (op (foo Z) A))
      (let* ([op-hash (op-set-ops (node-ops test-op-1))]
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
      (define-builtin-node test-op-2
        (include test-op)
        (op (foo B) Z))
      (let* ([op-hash (op-set-ops (node-ops test-op-2))]
             [foo-op (hash-ref op-hash 'foo)])
        (check-equal?
         (hash-count foo-op)  ; Number of different kinds
         1)
        (check-equal?
         (length (operator-signatures (hash-ref foo-op (list A-kind))))
         3))))

  (test-case "variable-arity"
    (let ()
      (define-builtin-node test-op-3
        (include test-op)
        (op (foo Z ...) A))
      (let* ([op-hash (op-set-ops (node-ops test-op-3))]
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

  (test-exn "undefined-range-sort"
      #rx"Undefined sort.*"
      (lambda ()
        (define-builtin-node test-op-4
          (op foo a))
        (void)))

  (test-exn "undefined-arg-sort"
      #rx"Undefined sort.*"
      (lambda ()
        (define-builtin-node test-op-5
          (sorts a)
          (op (foo b) a))
        (void)))

  (test-exn "preregularity"
      #rx"Operator .*bar.* is not preregular.*"
      (lambda ()
        (define-builtin-node test-op-6
          (include test-op)
          (op (bar C B) Z)
          (op (bar A C) Y))
        (void)))

  (test-exn "wrong-range-kind"
      #rx"Operator .*foo.* must have the kind of sort.*"
      (lambda ()
        (define-builtin-node test-op-7
          (include test-op)
          (op (foo B) A))
        (void)))

  (test-exn "mixed-var-args"
      #rx"Conflicting fixed and variable arity definitions.*"
      (lambda ()
        (define-builtin-node test-op-8
          (include test-op)
          (op (foo B ...) Z))
        (void)))

  (test-exn "double-definition"
      #rx"Signature .* already defined"
    (lambda ()
      (define-builtin-node test-op-9
        (include test-op)
        (op (foo A) X))
      (void)))
  
  (test-not-exn "direct and indirect import"
    (lambda ()
      (define-builtin-node test-op-10
        (include test-op))
      (define-builtin-node test-op-11
        (use test-op)
        (use test-op-10))
      (void)))
  
  (test-exn "restricted import"
      #rx"Operator .* was imported in restricted mode"
    (lambda ()
      (define-builtin-node test-op-12
        (use test-op)
        (op (foo B) Z))
      (void)))
  (test-not-exn "unrestricted import"
    (lambda ()
      (define-builtin-node test-op-13
        (include test-op)
        (op (foo B) Z))
      (void)))
  (test-not-exn "restricted and unrestricted import"
    (lambda ()
      (define-builtin-node test-op-14
        (use test-op))
      (define-builtin-node test-op-15
        (use test-op-14)
        (include test-op)
        (op (foo B) Z))
      (void))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests operator-tests))
