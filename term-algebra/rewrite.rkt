#lang racket

(provide reduce)

(require (prefix-in terms: term-algebra/terms)
         (prefix-in rules: term-algebra/rules)
         (prefix-in nodes: term-algebra/nodes)
         (prefix-in builtin: term-algebra/builtin))

(define true-term
  (nodes:make-term 'true empty builtin:truth))
(define false-term
  (nodes:make-term 'false empty builtin:truth))

; Term rewriting

(define (rewrite-head-once a-term node)
  (let* ([key (if (terms:term? a-term)
                  (terms:term-op a-term)
                  (terms:sort-of a-term))]
         [rules (hash-ref (nodes:node-rules node) key empty)]
         [ops (nodes:node-ops node)])
    (or (for/fold ([rewritten-term #f])
                  ([rule-with-origin rules])
          #:break rewritten-term
          (let ([rule (car rule-with-origin)])
            (if (procedure? rule)
                ; special rule (function)
                (with-handlers ([exn:fail? (lambda (v) #f)])
                  (apply rule (terms:term-args a-term)))
                ; rewrite rule
                (let* ([pattern (rules:rule-pattern rule)]
                       [condition (rules:rule-condition rule)]
                       [value (rules:rule-replacement rule)]
                       [s (terms:match-pattern pattern a-term ops)])
                  (if s
                      (if condition
                          (and (equal? (reduce (terms:substitute condition
                                                                 s ops)
                                               node)
                                       true-term)
                               (terms:substitute value s ops))
                          (terms:substitute value s ops))
                      #f)))))
        a-term)))

(define (rewrite-leftmost-innermost a-term node)
  (if (terms:term? a-term)
      (let* ([args (terms:term-args a-term)]
             [reduced-args (map (λ (arg) (reduce arg node)) args)]
             [with-reduced-args (if (andmap eq? args reduced-args)
                                    a-term
                                    (terms:make-term (terms:term-op a-term)
                                                     reduced-args
                                                     (nodes:node-ops node)))])
        (rewrite-head-once with-reduced-args node))
      (rewrite-head-once a-term node)))

(define (reduce a-term node)
  (let loop ([a-term a-term])
    (let* ([rewritten-term (rewrite-leftmost-innermost a-term node)])
      (if (eq? rewritten-term a-term)
          a-term
          (loop rewritten-term)))))
