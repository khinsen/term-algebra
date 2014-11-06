#lang racket

(provide (all-defined-out)
         (rename-out [module-begin #%module-begin])
         provide
         require submod)

(require (prefix-in terms: term-algebra/terms)
         (for-syntax syntax/parse))

(define-syntax (define-op stx)
  (syntax-parse stx
    [(_ op-name:id)
     #'(begin (define op-name (terms:op (quote op-name) '() '()))
              (provide op-name))]
    [(_ (op-name:id arg-name:id ...))
     #'(begin (define op-name (terms:op (quote op-name)
                                        (list (quote arg-name) ...) '()))
              (provide op-name))]))

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

(define-syntax (include stx)
  (syntax-parse stx
    [(_ module:expr)
     #'(begin (require module)
              (provide (all-from-out module)))]))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ decl:expr ...)
     #'(#%module-begin
        decl ...)]))
