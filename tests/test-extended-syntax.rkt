#lang racket

(provide extended-syntax-tests)

(require rackunit
         term-algebra/api
         (rename-in term-algebra/library/list [list list-section]))

(define-syntax-rule (check-reduce module initial-term reduced-term)
  (check-equal? (reduce (term module initial-term))
                (term module reduced-term)))

(define-section test-include
  (include list-section
           #:transforms (add-import (use builtin:string))
           (rename-sort Element String)
           (rename-sort List StringList)
           (rename-sort NonEmptyList NEStringList)
           (rename-op list string-list))
  (op bar NEStringList)
  (=-> bar (string-list "a" "b" "c")))

(define-section test-cond
  (use builtin:equality)
  (sort A)
  (op (foo A) A)
  (op bar A)
  (op baz A)
  (=-> #:var [AA A]
       (foo AA)
       #:cond [(== AA bar) bar]
              [#:else baz]))

(define-section test-vars
  (use builtin:equality)
  (sort A)
  (op (foo A) A)
  (op bar A)
  (op baz A)
  (vars [AA A])
  (=-> (foo AA)
       #:cond [(== AA bar) bar]
              [#:else baz]))

(define-test-suite extended-syntax-tests

  (check-reduce test-include (tail bar) (string-list "b" "c"))

  (check-reduce test-cond (foo bar) bar)
  (check-reduce test-cond (foo baz) baz)
  (check-reduce test-cond (foo (foo bar)) bar)

  (check-reduce test-vars (foo bar) bar)
  (check-reduce test-vars (foo baz) baz)
  (check-reduce test-vars (foo (foo bar)) bar)

  (test-exn "redefinition 1"
      #rx"Redefinition of variable\\(s\\)"
      (lambda ()
        (define-section redef1
          (sort Foo)
          (vars [aFoo Foo])
          (vars [aFoo Foo]))
        (void)))
  (test-exn "redefinition 2"
      #rx"Redefinition of variable\\(s\\)"
      (lambda ()
        (define-section redef1
          (sort Foo)
          (op (foo Foo) Foo)
          (op bar Foo)
          (vars [aFoo Foo])
          (=-> #:var [aFoo Foo] (foo bar) bar))
        (void))))

(module* main #f
  (require rackunit/text-ui)
  (run-tests extended-syntax-tests))
