#lang racket

(provide (struct-out term)
         (struct-out var)
         vars-in-term
         sort-of
         make-term make-special-term)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators))

; Struct definitions

(struct term (op args sort op-set)
        #:transparent
        #:property prop:custom-write
        (lambda (term port mode)
          (let ([op (term-op term)])
            (if (or (not (null? (term-args term)))
                    (operators:has-var-arity? op (term-op-set term)))
                (write (cons op (term-args term)) port)
                (write op port)))))

(struct var (symbol sort sigature)
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

(define (make-term op args op-set)
  ;; (unless (andmap (λ (t) (equal? (term-op-set t) op-set)) args)
  ;;   (error "Argument terms defined in a different module"))
  (unless (operators:has-op? op op-set)
    (error "Undefined operator " op))
  (let ([sort
         (operators:lookup-op op
                              (map sort-of args)
                              op-set)])
    (unless sort
      (error "Wrong number or sort of arguments: " (cons op args)))
    (term op args sort op-set)))

(define (make-special-term value op-set)
  (cond
   [(string? value)
    (if (operators:has-special-op? 'string op-set)
        value
        (error "import builtin:string to use strings"))]
   [(symbol? value)
    (if (operators:has-special-op? 'symbol op-set)
        value
        (error "import builtin:symbol to use symbols"))]
   [(number? value)
    (if (operators:has-special-op? 'rational-number op-set)
        value
        (error "import builtin:rational to use rational numbers"))]
   [else (error "invalid special term " value)]))
