#lang racket

(provide (struct-out term)
         (struct-out var)
         vars-in-term
         sort-of
         make-term make-pattern make-special-term)

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

(struct var (symbol sort)
        #:transparent
        #:property prop:custom-write
        (lambda (var port mode)
          (begin
            (write (var-symbol var) port)
            (write-string ":" port)
            (write (var-sort var) port))))

; Basic operations

(define (vars-in-term term)
  (cond
   [(var? term)  (set term)]
   [(term? term) (let ([args (term-args term)])
                   (if (empty? args)
                       (set)
                       (apply set-union (map vars-in-term args))))]
   [else         (set)]))

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

(define (make-pattern op args op-set vars)
  (if (and (empty? args)
           (hash-has-key? vars op))
      (let ([sort (hash-ref vars op)])
        (unless (sorts:has-sort? sort (operators:op-set-sorts op-set))
          (error "Undefined sort " sort))
        (var op sort))
      (make-term op args op-set)))

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
