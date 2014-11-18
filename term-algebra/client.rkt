#lang racket

(provide (all-defined-out))

(require (prefix-in terms: term-algebra/terms)
         (for-syntax syntax/parse))

(define (sexpr->term module sexp)
  (let ([ops (hash-ref module 'ops)]
        [vars (hash-ref module 'vars)])
    (define (->term sexp)
      (cond
       [(and (symbol? sexp) (hash-has-key? ops sexp))
        (terms:term (hash-ref ops sexp) '())]
       [(and (symbol? sexp) (hash-has-key? vars sexp))
        (hash-ref vars sexp)]
       [(symbol? sexp)
        (error "unbound symbol" sexp)]
       [(not (list? sexp))
        (error "illegal syntax" sexp)]
       [(not (symbol? (first sexp)))
        (error "illegal syntax" (first sexp))]
       [(not (hash-has-key? ops (first sexp)))
        (error "unbound symbol" (first sexp))]
       [else
        (let ([op (hash-ref ops (first sexp))]
              [args (map ->term (rest sexp))])
          (if (equal? (length args) (length (terms:op-args op)))
              (terms:term op args)
              (error "wrong number of arguments for " op)))]))
    (->term sexp)))

(define-syntax (term stx)
  (syntax-parse stx
    [(_ module:expr sexp:expr)
     #'(sexpr->term module (quote sexp))]))

(define-syntax (use-module stx)
  (syntax-parse stx
    [(_ module-name:id)
     (with-syntax ([module-id (datum->syntax stx 'module)])
       #'(define module-name (let () (local-require module-name) module-id)))]
    [(_ module-name:id module-expr:expr)
     (with-syntax ([module-id (datum->syntax stx 'module)])
       #'(define module-name (let () (local-require module-expr) module-id)))]))
