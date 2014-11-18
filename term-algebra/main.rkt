#lang racket

(provide (all-defined-out)
         (rename-out [module-begin #%module-begin])
         provide
         require submod)

(require (prefix-in terms: term-algebra/terms)
         (for-syntax syntax/parse))

(define (syntax-info stx)
  (list (syntax-source stx) (syntax-line stx) (syntax-column stx)))

(define-syntax (define-op stx)
  (syntax-parse stx
    [(_ op-name:id)
     (with-syntax ([ops-id (datum->syntax stx (string->unreadable-symbol "ops"))])
       #'(begin ;(define op-name (terms:op (quote op-name) '() '()))
           (set! ops-id (hash-set ops-id
                                  (quote op-name)
                                  (cons (syntax-info #'op-name) (quote op-name))))))]
    [(_ (op-name:id arg-name:id ...))
     (with-syntax ([ops-id (datum->syntax stx (string->unreadable-symbol "ops"))])
       #'(begin ;(define op-name (terms:op (quote op-name)
                ;                     (list (quote arg-name) ...) '()))
           (set! ops-id (hash-set ops-id
                                  (quote op-name)
                                  (cons (syntax-info op-name) (list (quote op-name) (quote arg-name) ...))))))]))

(define-syntax (define-var stx)
  (syntax-parse stx
    [(_ var-name:id)
     (with-syntax ([vars-id (datum->syntax stx
                                           (string->unreadable-symbol "vars"))])
       #'(begin ;(define var-name (terms:var (quote var-name)))
           (set! vars-id (hash-set vars-id
                                   (quote var-name)
                                   (cons (syntax-info #'var-name) (quote var-name))))))]))

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
             #:with value #'(terms:term op (list args.value ...))))
  
  (define-syntax-class condition
    #:description "condition"
    (pattern (== term1:term term2:term)
             #:with value #'(terms:term terms:builtin:==
                                        (list term1.value term2.value)))))

(define-syntax (=-> stx)
  (syntax-parse stx
    [(_ left:term right:term)
     #'(let* ([l left.value]
              [r right.value]
              [rule (cons l r)]
              [op (terms:term-op l)])
         (terms:set-op-rules! op (append (terms:op-rules op) (list rule))))]
    [(_ left:term right:term #:if cond:condition)
     #'(let* ([l left.value]
              [r right.value]
              [c cond.value]
              [rule (cons l (cons c r))]
              [op (terms:term-op l)])
         (terms:set-op-rules! op (append (terms:op-rules op) (list rule))))]))

(define-syntax (include stx)
  (syntax-parse stx
    [(_ module-expr:expr)
     (with-syntax ([includes-id (datum->syntax stx
                                               (string->unreadable-symbol "includes"))])
       #'(set! includes-id (set-add includes-id
                                    (cons (syntax-info #'module-expr) (quote module-expr)))))]))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ decl:expr ...)
     (with-syntax ([module-id (datum->syntax stx 'module)]
                   [includes-id (datum->syntax stx
                                          (string->unreadable-symbol "includes"))]
                   [ops-id (datum->syntax stx
                                          (string->unreadable-symbol "ops"))]
                   [vars-id (datum->syntax stx
                                           (string->unreadable-symbol "vars"))])
       #'(#%module-begin
          (define module-id
            (let ([includes-id (set)]
                  [ops-id (hash)]
                  [vars-id (hash)])
              decl ...
              (hash 'includes includes-id 'ops ops-id 'vars vars-id)))
          (provide module-id)))]))
