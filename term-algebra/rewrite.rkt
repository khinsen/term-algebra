#lang racket

(provide reduce)

(require term-algebra/terms
         (only-in term-algebra/modules op-from)
         (prefix-in builtin: term-algebra/builtin))

(define true-op (op-from builtin:truth 'true))
(define true-term (term true-op '()))
(define false-op (op-from builtin:truth 'false))
(define false-term (term false-op '()))

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
    (cond
     [(var? p)
      (hash-ref s p)]
     [(term? p)
      (term (term-op p) (for/list ([a (term-args p)]) (substitute a s)))]
     [else p]))
  
  (let ([rules (op-rules (term-op a-term))])
    (if (procedure? rules)
        ; special operator
        (with-handlers ([exn:fail? (lambda (v) a-term)])
          (apply rules (term-args a-term)))
        ; rule list
        (or (for/fold ([rewritten-term #f])
                      ([rule rules])
              #:break rewritten-term
              (let* ([pattern (car rule)]
                     [rest (cdr rule)]
                     [condition (if (pair? rest) (car rest) #f)]
                     [value (if (pair? rest) (cdr rest) rest)]
                     [s (match-pattern pattern a-term)])
                (if s
                    (if condition
                        (and (equal? (reduce (substitute condition s))
                                     true-term)
                             (substitute value s))
                        (substitute value s))
                    #f)))
            a-term))))

(define (rewrite-leftmost-innermost a-term)
  (if (term? a-term)
      (let* ([args (term-args a-term)]
             [reduced-args (map reduce args)]
             [with-reduced-args (if (andmap eq? args reduced-args)
                                    a-term
                                    (term (term-op a-term) reduced-args))])
        (rewrite-head-once with-reduced-args))
      a-term))

(define (reduce a-term)
  (let loop ([a-term a-term])
    (let* ([rewritten-term (rewrite-leftmost-innermost a-term)])
      (if (eq? rewritten-term a-term)
          a-term
          (loop rewritten-term)))))
