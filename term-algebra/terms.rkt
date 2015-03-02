#lang racket

(provide (struct-out term)
         (struct-out var)
         vars-in-term
         sort-of
         make-term make-ht-pattern make-special-term
         match-pattern substitute
         op-origin)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators))

; Struct definitions

(struct term (op args sort)
        #:transparent
        #:property prop:custom-write
        (lambda (term port mode)
          (let ([op (term-op term)])
            (if (null? (term-args term))
                (write op port)
                (write (cons op (term-args term)) port)))))

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
                   (cond
                     [(empty? args) (set)]
                     [(list? args) (apply set-union (map vars-in-term args))]
                     [else (set-union (vars-in-term (car args))
                                      (vars-in-term (cdr args)))]))]
   [else         (set)]))

(define (sort-of gterm)
  (cond
   [(term? gterm)   (term-sort gterm)]
   [(var? gterm)    (var-sort gterm)]
   [(symbol? gterm) 'Symbol]
   [(string? gterm) 'String]
   [(and (number? gterm) (exact? gterm))
    (cond
      [(zero? gterm) 'Zero]
      [(integer? gterm) (if (positive? gterm) 'NonZeroNatural 'NonZeroInteger)]
      [else (if (positive? gterm) 'PositiveRational 'NonZeroRational)])]
   [else (error "unknown term type" gterm)]))

(define (range-sort op args op-set)
  (operators:lookup-range op (map sort-of args) op-set))

(define (make-term op args op-set)
  (unless (operators:has-op? op op-set)
    (error "Undefined operator " op))
  (let ([sort (range-sort op args op-set)])
    (unless sort
      (error "Wrong number or sort of arguments: " (cons op args)))
    (term op args sort)))

(define (make-ht-pattern op head tail op-set)
  ; TODO Should check more carefully that the resulting term has a well-defined sort.
  (unless (operators:has-op? op op-set)
    (error "Undefined operator " op))
  (let ([sort
         (operators:lookup-var-arity-range
          op (sort-of head) (sort-of tail) op-set)])
    (unless sort
      (error "Wrong number or sort of arguments: " (cons op (cons head tail))))
    (term op (cons head tail) sort)))

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
   [(and (number? value) (exact? value))
    (if (operators:has-special-op? 'rational-number op-set)
        value
        (if (and (integer? value)
                 (operators:has-special-op? 'integer-number op-set))
            value
            (if (and (not (negative? value))
                     (operators:has-special-op? 'natural-number op-set))
                value
                (if (not (integer? value))
                    (error "import builtin:rational to use rational numbers")
                    (if (negative? value)
                        (error "import builtin:integer to use integer numbers")
                        (error "import builtin:natural to use natural numbers"))))))]
   [else (error "invalid special term " value)]))

; Pattern matching and substitution

(define (match-pattern pattern target op-set)

  (define (merge-substitutions s-acc s)
    (if (not s)
        #f
        (for/fold ([s-acc s-acc])
                  ([var (hash-keys s)])
          #:break (not s-acc)
          (let ([value (hash-ref s var)])
            (if (and (hash-has-key? s-acc var)
                     (not (equal? (hash-ref s-acc var) value)))
                #f
                (hash-set s-acc var value))))))

  (define (match-pattern* pattern target sorts)
    (cond
     [(var? pattern)
      (if (sorts:is-sort? (sort-of target) (var-sort pattern) sorts)
          (hash pattern target)
          #f)]
     [(and (term? pattern)
           (term? target)
           (equal? (term-op pattern) (term-op target)))
      (let ([p-args (term-args pattern)]
            [t-args (term-args target)])
        (if (list? p-args)
            ; The pattern is a standard term
            (if (equal? (length p-args) (length t-args))
                (for/fold ([subst (hash)])
                          ([p-arg p-args]
                           [t-arg t-args])
                  #:break (not subst)
                  (merge-substitutions subst
                                       (match-pattern* p-arg t-arg sorts)))
                #f)
            ; The pattern is a head-tail term
            (let ([sort (range-sort (term-op target) (rest t-args) op-set)])
              (and sort
                   (merge-substitutions
                    (match-pattern* (car p-args) (first t-args) sorts)
                    (match-pattern* (cdr p-args)
                                    (term (term-op target) (rest t-args) sort)
                                    sorts))))))]
     [(equal? pattern target)
      (hash)]
     [else
      #f]))

  (match-pattern* pattern target (operators:op-set-sorts op-set)))

(define (substitute pattern substitution op-set)
  (cond
   [(var? pattern)
    (hash-ref substitution pattern)]
   [(term? pattern)
    (let* ([op-symbol (term-op pattern)]
           [sort (operators:lookup-range
                  op-symbol (map sort-of (term-args pattern)) op-set)])
      (term op-symbol
            (map (λ (arg) (substitute arg substitution op-set))
                 (term-args pattern))
            sort))]
   [else pattern]))

(define (op-origin term op-set)
  (let ([op (term-op term)]
        [args (term-args term)])
    (if (list? args)
        (operators:lookup-origin op (map sort-of args) op-set)
        (operators:lookup-var-arity-origin
         op (sort-of (car args)) (sort-of (cdr args)) op-set))))
