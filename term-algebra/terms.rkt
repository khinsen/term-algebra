#lang racket

(provide (struct-out term)
         (struct-out var)
         vars-in-term
         sort-of
         make-term)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in modules: term-algebra/modules))

; Struct definitions

(struct term (op args sort module)
        #:transparent
        #:property prop:custom-write
        (lambda (term port mode)
          (let ([op (term-op term)])
            (if (null? (term-args term))
                (write op port)
                (write (cons op (term-args term)) port)))))

(struct var (symbol sort module)
        #:transparent
        #:property prop:custom-write
        (lambda (var port mode)
          (write (list (var-symbol var) (var-sort var)) port)))

; Basic operations

(define (vars-in-term term)
  (cond
   [(var? term)  (seteq term)]
   [(term? term) (let ([args (term-args term)])
                   (if (empty? args)
                       (seteq)
                       (apply set-union (map vars-in-term args))))]
   [else         (seteq)]))

(define (sort-of gterm)
  (cond
   [(term? gterm)   (term-sort gterm)]
   [(var? gterm)    (var-sort gterm)]
   [(symbol? gterm) 'Symbol]
   [(string? gterm) 'String]
   [(number? gterm) 'Rational]
   [else (error "unknown term type" gterm)]))

(define (make-term op args module)
  (unless (andmap (λ (t) (equal? (term-module t) module)) args)
    (error "Argument terms defined in a different module"))
  (unless (operators:has-op? op (modules:module-ops module))
    (error "Undefined operator " op))
  (let ([sort
         (operators:lookup-op op
                              (map sort-of args)
                              (modules:module-ops module))])
    (unless sort
      (error "Wrong number or sort of arguments: " (cons op args)))
    (term op args sort module)))
