#lang racket

(provide term term? term-op term-args
         (struct-out var)
         (struct-out svar)
         vars-in-term
         sort-of
         make-term make-pattern make-special-term
         term-hashcode
         match-pattern substitute
         op-origin)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (only-in file/sha1 sha1))

; Struct definitions

(struct term (op args signature)
        #:transparent
        #:constructor-name -term
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

(struct svar (symbol sort)
        #:transparent
        #:property prop:custom-write
        (lambda (svar port mode)
          (begin
            (write (svar-symbol svar) port)
            (write-string ":" port)
            (write (svar-sort svar) port)
            (write-string "..." port))))

; Basic operations

(define (vars-in-term term)
  (cond
    [(or (var? term) (svar? term))
     (set term)]
    [(term? term)
     (let ([args (term-args term)])
       (if (empty? args)
           (set)
           (apply set-union (map vars-in-term args))))]
    [else (set)]))

(define (sort-of gterm)
  (cond
   [(term? gterm)   (operators:signature-range (term-signature gterm))]
   [(var? gterm)    (var-sort gterm)]
   [(svar? gterm)   (svar-sort gterm)]
   [(symbol? gterm) 'Symbol]
   [(string? gterm) 'String]
   [(and (number? gterm) (exact? gterm))
    (cond
      [(zero? gterm) 'Zero]
      [(integer? gterm) (if (positive? gterm) 'NonZeroNatural 'NonZeroInteger)]
      [else (if (positive? gterm) 'PositiveRational 'NonZeroRational)])]
   [else (error "unknown term type" gterm)]))

(define (make-term op args op-set)
  (unless (operators:has-op? op op-set)
    (error "Undefined operator " op))
  (let ([signature (operators:lookup-op op (map sort-of args) op-set)])
    (unless signature
      (error "Wrong number or sort of arguments: " (cons op args)))
    (-term op args signature)))

(define (make-pattern op args op-set)
  (unless (operators:has-op? op op-set)
    (error "Undefined operator " op))
  (let* ([nsvars (count svar? args)]
         [signature (cond
                      [(zero? nsvars)
                       (operators:lookup-op op (map sort-of args) op-set)]
                      [(and (equal? nsvars 1)
                            (svar? (last args)))
                       (operators:lookup-var-arity-op
                        op (map sort-of args) op-set)]
                      [else
                       (error "svar allowed only as last argument")])])
    (unless signature
      (error (format "No signature for ~s" (cons op args))))
    (-term op args signature)))

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

(define (term-hashcode a-term-or-string)
  (define (hash-of-string s)
    (sha1 (open-input-string s)))
  (let ([o (open-output-string)])
    (write a-term-or-string o)
    (hash-of-string (get-output-string o))))

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

  (define (match-var var target sorts)
    (if (sorts:is-sort? (sort-of target) (var-sort var) sorts)
        (hash var target)
        #f))

  (define (match-svar svar targets sorts)
    (if (andmap (λ (t) (sorts:is-sort? (sort-of t) (svar-sort svar) sorts))
                targets)
        (hash svar targets)
        #f))

  (define (match-args p-args t-args sorts)
    (cond
      [(and (not (empty? p-args))
            (svar? (last p-args)))
       ; The pattern has an svar argument
       (cond
         [(< (length t-args) (- (length p-args) 1))
          ; Not enough arguments
          #f]
         [else
          (define n-fix (- (length p-args) 1))
          (for/fold ([subst (match-svar
                             (last p-args) (drop t-args n-fix) sorts)])
                    ([p-arg (take p-args n-fix)]
                     [t-arg (take t-args n-fix)])
            #:break (not subst)
            (merge-substitutions subst
                                 (match-pattern* p-arg t-arg sorts)))])
       ]
      [else
       ; The pattern is a standard term
       (cond
         [(equal? (length p-args) (length t-args))
          ; Matching number of arguments
          (for/fold ([subst (hash)])
                    ([p-arg p-args]
                     [t-arg t-args])
            #:break (not subst)
            (merge-substitutions subst
                                 (match-pattern* p-arg t-arg sorts)))]
         [else #f])]))
  
  (define (match-pattern* pattern target sorts)
    (cond
      [(var? pattern)
       (match-var pattern target sorts)]
      [(and (term? pattern)
            (term? target)
            (equal? (term-op pattern) (term-op target)))
       (match-args (term-args pattern) (term-args target) sorts)]
      [(equal? pattern target)
       (hash)]
      [else
       #f]))

  (match-pattern* pattern target (operators:op-set-sorts op-set)))

(define (substitute pattern substitution op-set)
  (cond
    ; svar
   [(var? pattern)
    (hash-ref substitution pattern)]
   [(term? pattern)
    (let* ([op-symbol (term-op pattern)]
           [p-args (term-args pattern)]
           [args (if (and (not (empty? p-args)) (svar? (last p-args)))
                     (append (map (λ (arg) (substitute arg substitution op-set))
                                  (drop-right p-args 1))
                             (hash-ref substitution (last p-args)))
                     (map (λ (arg) (substitute arg substitution op-set))
                          p-args))]
           [signature (operators:lookup-op
                       op-symbol (map sort-of args) op-set)])
      (-term op-symbol args signature))]
   [else pattern]))

(define (op-origin term)
  (operators:signature-origin (term-signature term)))
