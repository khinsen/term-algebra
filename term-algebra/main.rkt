#lang racket

(provide (all-defined-out)
         #%module-begin
         provide all-defined-out all-from-out
         require submod)

(require (prefix-in terms: term-algebra/terms)
         (for-syntax syntax/parse))

(define-syntax (define-op stx)
  (syntax-parse stx
    [(_ op-name:id)
     #'(define op-name (terms:op (quote op-name) '() '()))]
    [(_ (op-name:id arg-name:id ...))
     #'(define op-name (terms:op (quote op-name)
                                 (list (quote arg-name) ...) '()))]))

(define-syntax (define-var stx)
  (syntax-parse stx
    [(_ var-name:id)
     #'(define var-name (terms:var (quote var-name)))]))

(define-syntax (define-vars stx)
  (syntax-parse stx
    [(_ var-name:id ...)
     #'(begin (define var-name (terms:var (quote var-name)))
              ...)]))

(begin-for-syntax
  
  (define-syntax-class term
    #:description "term"
    (pattern symbol:id
             #:with value #'(if (terms:var? symbol)
                                symbol
                                (terms:term symbol '())))
    (pattern (op:id args:term ...)
             #:with value #'(terms:term op (list args.value ...)))))

(define-syntax (term stx)
  (syntax-parse stx
    [(_ t:term)
     #'t.value]))

(define-syntax (define-term stx)
  (syntax-parse stx
    [(_ symbol:id t:term)
     #'(define symbol t.value)]))

(define-syntax (=-> stx)
  (syntax-parse stx
    [(_ left:term right:term)
     #'(let* ([l left.value]
              [r right.value]
              [rule (cons l r)]
              [op (terms:term-op l)])
         (terms:set-op-rules! op (append (terms:op-rules op) (list rule))))]))
