#lang racket

(provide (struct-out op) (struct-out var) (struct-out term)
         reduce)

; Struct definitions

(struct op (symbol args [rules #:mutable])
        #:transparent
        #:property prop:custom-write
        (lambda (op port mode)
          (if (null? (op-args op))
              (write (op-symbol op) port)
              (write (cons (op-symbol op) (op-args op)) port))))

(struct var (symbol)
        #:transparent
        #:property prop:custom-write
        (lambda (var port mode)
          (write (var-symbol var) port)))

(struct term (op args)
        #:transparent
        #:property prop:custom-write
        (lambda (term port mode)
          (let ([op (term-op term)])
            (if (null? (op-args op))
                (write (op-symbol op) port)
                (write (cons (op-symbol op) (term-args term)) port)))))

; Term rewriting

(define (rewrite-head-once a-term)
  
  (define (match-pattern p t)

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
    
    (cond
     [(var? p) (hash p t)]
     [(not (equal? (term-op p) (term-op t))) #f]
     [else (for/fold ([s (hash)])
                     ([p-arg (term-args p)]
                      [t-arg (term-args t)])
             #:break (not s)
             (merge-substitutions s (match-pattern p-arg t-arg)))]))
  
  (define (substitute p s)
    (if (var? p)
        (hash-ref s p)
        (term (term-op p) (for/list ([a (term-args p)]) (substitute a s)))))
  
  (or (for/fold ([rewritten-term #f])
                ([rule (op-rules (term-op a-term))])
        #:break rewritten-term
        (let ([s (match-pattern (car rule) a-term)])
          (if s
              (substitute (cdr rule) s)
              #f)))
      a-term))

(define (rewrite-leftmost-innermost a-term)
  (let* ([args (term-args a-term)]
         [reduced-args (map reduce args)]
         [with-reduced-args (if (andmap eq? args reduced-args)
                                a-term
                                (term (term-op a-term) reduced-args))])
    (rewrite-head-once with-reduced-args)))

(define (reduce a-term)
  (let loop ([a-term a-term])
    (let* ([rewritten-term (rewrite-leftmost-innermost a-term)])
      (if (eq? rewritten-term a-term)
          a-term
          (loop rewritten-term)))))
