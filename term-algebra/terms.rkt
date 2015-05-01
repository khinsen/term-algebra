#lang racket

(provide term term? term-op term-args
         (struct-out var)
         (struct-out svar)
         vars-in-term
         sort-of
         make-term make-pattern make-special-term
         term-hashcode
         term-equal?
         match-pattern substitute
         op-origin)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (only-in file/sha1 sha1)
         racket/generator)

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

(struct svar (symbol sort allow-zero?)
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
                       (let ([sig
                              (operators:lookup-var-arity-op
                               op (map sort-of args) op-set)])
                         (when (and sig
                                    (equal? 1 (length args))
                                    (svar-allow-zero? (last args)))
                           ; A single arg which is an svar allowing
                           ; zero arguments: valid only if a zero-arg
                           ; version of the operator exists as well.
                           (unless (operators:lookup-op op empty op-set)
                             (error (format "No zero-argument form for ~s" op))))
                         sig)]
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


; Racket's built-in in-permutations doesn't make any promises about
; the order of returned permutations. This version guarantees that
; the original list is the first permutation. It is used for matching
; on symmetric operators.
(define (in-perms l)
  (in-generator
   (if (empty? l)
       (yield empty)
       (for ([x l])
         (for ([ys (in-perms (remq x l))])
           (yield (cons x ys)))))))

; Term equality

(define (term-equal? term1 term2)

  (define (compare-args args1 args2)
    (and (equal? (length args1) (length args2))
         (andmap term-equal? args1 args2)))

  (define (compare-symmetric-args args1 args2)
    (and (equal? (length args1) (length args2))
         (or (empty? args1)
             (compare-symmetric-args (rest args1)
                                     (remove (first args1) args2
                                             term-equal?)))))

  (cond
    [(and (term? term1) (term? term2))
     (and (equal? (term-op term1) (term-op term2))
          (if (operators:signature-symmetric? (term-signature term2))
              (compare-symmetric-args (term-args term1) (term-args term2))
              (compare-args (term-args term1) (term-args term2))))]
    [else (equal? term1 term2)]))

; Pattern matching and substitution

(define (match-pattern pattern target op-set)

  (define sorts (operators:op-set-sorts op-set))
  (define (is-sort? term target-sort)
    (sorts:is-sort? (sort-of term) target-sort sorts))

  (define (single-match condition value)
    (if condition
        (in-value value)
        empty-sequence))

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

  (define (match-fixed-args p-args t-args substitution match-tail)
    (if (empty? p-args)
        (match-tail t-args substitution)
        (in-generator
         (for ([sf (match-pattern* (first p-args) (first t-args))])
           (let ([sm (merge-substitutions substitution sf)])
             (when sm
               (for ([s (match-fixed-args (rest p-args) (rest t-args)
                                          sm match-tail)])
                 (yield s))))))))

  (define (match-svar-args fixed-p-args svar t-args)

    (define (match-svar t-args substitution)
      (cond
        [(andmap (λ (t) (is-sort? t (svar-sort svar))) t-args)
         (define s (merge-substitutions substitution (hash svar t-args)))
         (single-match s s)]
        [else
         empty-sequence]))
    (if (< (length t-args)
           (+ (length fixed-p-args) (if (svar-allow-zero? svar) 0 1)))
        empty-sequence
        (match-fixed-args fixed-p-args t-args (hash) match-svar)))

  (define (match-plain-args p-args t-args)
    (if (equal? (length p-args) (length t-args))
        (match-fixed-args p-args t-args (hash)
                          (λ (t s) (in-value s)))
        empty-sequence))

  (define (match-args p-args t-args)
    (cond
      [(empty? p-args)
       (single-match (empty? t-args) (hash))]
      [(svar? (last p-args))
       (let-values ([(fixed svar-list) (split-at-right p-args 1)])
         (match-svar-args fixed (first svar-list) t-args))]
      [else
       (match-plain-args p-args t-args)]))

  (define (match-symmetric-args p-args t-args)
    ; A brute-force implementation that is probably very inefficient.
    (define length-match
      (if (and (not (empty? p-args))
               (svar? (last p-args)))
          (>= (length t-args) (- (length p-args) 1))
          (equal? (length p-args) (length t-args))))
    (if length-match
        (in-generator
         (for* ([pt (in-perms t-args)]
                [s (match-args p-args pt)])
           (yield s)))
        empty-sequence))

  (define (match-pattern* pattern target)
    (cond
      [(var? pattern)
       (single-match (is-sort? target (var-sort pattern))
                     (hash pattern target))]
      [(term? pattern)
       (if (and (term? target)
                  (equal? (term-op pattern) (term-op target)))
         (if (operators:signature-symmetric? (term-signature target))
             (match-symmetric-args (term-args pattern)
                                   (term-args target))
             (match-args (term-args pattern) (term-args target)))
         empty-sequence)]
      [else
       (single-match (equal? pattern target) (hash))]))

  (match-pattern* pattern target))

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
