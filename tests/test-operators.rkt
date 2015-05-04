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

(define (lookup-range symbol arg-sorts ops)
  (let ([op-sig (operators:lookup-op symbol arg-sorts ops)])
    (and op-sig
         (operators:signature-range op-sig))))

(define (lookup-var-arity-range symbol arg-sorts ops)
  (let ([op-sig (operators:lookup-var-arity-op symbol arg-sorts ops)])
    (and op-sig
         (operators:signature-range op-sig))))

(define-builtin-node test-op
  (sorts A B C D E X Y Z)
  (subsorts [A C] [B C] [C E] [D E] [X Y] [X Z])
  (op (foo A) X)
  (op (foo C) Z))

(define test-sorts (node-sorts test-op))
(define test-ops (node-ops test-op))
(define A-kind (sorts:kind 'A test-sorts))
(define X-kind (sorts:kind 'X test-sorts))

(define-builtin-node test-var-op
  (sorts A B C D E X Y Z)
  (subsorts [A C] [B C] [C E] [D E] [X Y] [X Z])
  (op (foo A ...) X)
  (op (foo C ...) Z))

(define test-var-sorts (node-sorts test-var-op))
(define test-var-ops (node-ops test-var-op))
(define A-kind-var (sorts:kind 'A test-var-sorts))
(define X-kind-var (sorts:kind 'X test-var-sorts))

(define-builtin-node test-op-any
  (sorts Any X Y)
  (op (foo Any) X)
  (op (foo X Any) Y)
  (op (bar Any ...) X))

(define test-any-ops (node-ops test-op-any))

(define-test-suite operator-tests

  (test-case "test-sorts"
    (check-equal? A-kind (set 'A 'B 'C 'D 'E))
    (check-equal? X-kind (set 'X 'Y 'Z)))

  (test-case "operator-lookup"
    (check-equal? (lookup-range 'foo (list 'A) test-ops)
                  'X)
    (check-equal? (lookup-range 'foo (list 'B) test-ops)
                  'Z)
    (check-equal? (lookup-range 'foo (list 'C) test-ops)
                  'Z)
    (check-equal? (lookup-range 'foo (list 'D) test-ops)
                  X-kind)
    (check-equal? (lookup-range 'foo (list 'E) test-ops)
                  X-kind)
    (check-equal? (lookup-range 'foo (list A-kind) test-ops)
                  X-kind)
    (check-equal? (lookup-range 'foo (list 'X) test-ops)
                  #f)
    (check-equal? (lookup-range 'foo (list 'Y) test-ops)
                  #f)
    (check-equal? (lookup-range 'foo (list 'Z) test-ops)
                  #f))

  (test-case "operator-lookup-var"
    (check-equal? (lookup-range 'foo (list 'A) test-var-ops)
                  'X)
    (check-equal? (lookup-range 'foo (list 'B) test-var-ops)
                  'Z)
    (check-equal? (lookup-range 'foo (list 'C) test-var-ops)
                  'Z)
    (check-equal? (lookup-range 'foo (list 'D) test-var-ops)
                  X-kind)
    (check-equal? (lookup-range 'foo (list 'E) test-var-ops)
                  X-kind)
    (check-equal? (lookup-range 'foo (list A-kind) test-var-ops)
                  X-kind)
    (check-equal? (lookup-range 'foo (list 'X) test-var-ops)
                  #f)
    (check-equal? (lookup-range 'foo (list 'Y) test-var-ops)
                  #f)
    (check-equal? (lookup-range 'foo (list 'Z) test-var-ops)
                  #f))

  (test-case "operator-lookup-var-arity"
    (check-equal? (lookup-var-arity-range 'foo (list 'A) test-var-ops)
                  'X)
    (check-equal? (lookup-var-arity-range 'foo (list 'B) test-var-ops)
                  'Z)
    (check-equal? (lookup-var-arity-range 'foo (list 'C) test-var-ops)
                  'Z)
    (check-equal? (lookup-var-arity-range 'foo (list 'D) test-var-ops)
                  X-kind)
    (check-equal? (lookup-var-arity-range 'foo (list 'E) test-var-ops)
                  X-kind)
    (check-equal? (lookup-var-arity-range 'foo (list A-kind) test-var-ops)
                  X-kind)
    (check-equal? (lookup-var-arity-range 'foo (list 'X) test-var-ops)
                  #f)
    (check-equal? (lookup-var-arity-range 'foo (list 'Y) test-var-ops)
                  #f)
    (check-equal? (lookup-var-arity-range 'foo (list 'Z) test-var-ops)
                  #f))

  (test-case "operator-lookup-any"
    (check-equal? (lookup-range 'foo (list 'X) test-any-ops)
                  'X)
    (check-equal? (lookup-range 'foo (list 'X 'X) test-any-ops)
                  'Y)
    (check-equal? (lookup-range 'bar empty test-any-ops)
                  #f)
    (check-equal? (lookup-range 'bar (list 'X) test-any-ops)
                  'X)
    (check-equal? (lookup-range 'bar (list 'X 'X) test-any-ops)
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
