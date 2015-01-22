#lang racket

(provide reduce)

(require (prefix-in terms: term-algebra/terms)
         (prefix-in rules: term-algebra/rules)
         (prefix-in modules: term-algebra/modules)
         (prefix-in builtin: term-algebra/builtin))

(define true-term
  (modules:make-term 'true empty builtin:truth))
(define false-term
  (modules:make-term 'false empty builtin:truth))

; Term rewriting

(define (rewrite-head-once a-term module)
  (let* ([key (if (terms:term? a-term)
                  (terms:term-op a-term)
                  (terms:sort-of a-term))]
         [rules (hash-ref (modules:module-rules module) key empty)]
         [ops (modules:module-ops module)])
    (if (procedure? rules)
        ; special operator
        (with-handlers ([exn:fail? (lambda (v) a-term)])
          (apply rules (terms:term-args a-term)))
        ; rule list
        (or (for/fold ([rewritten-term #f])
                      ([rule rules])
              #:break rewritten-term
              (let* ([pattern (rules:rule-pattern rule)]
                     [condition (rules:rule-condition rule)]
                     [value (rules:rule-replacement rule)]
                     [s (terms:match-pattern pattern a-term ops)])
                (if s
                    (if condition
                        (and (equal? (reduce (terms:substitute condition s ops)
                                             module)
                                     true-term)
                             (terms:substitute value s ops))
                        (terms:substitute value s ops))
                    #f)))
            a-term))))

(define (rewrite-leftmost-innermost a-term module)
  (if (terms:term? a-term)
      (let* ([args (terms:term-args a-term)]
             [reduced-args (map (λ (arg) (reduce arg module)) args)]
             [with-reduced-args (if (andmap eq? args reduced-args)
                                    a-term
                                    (terms:make-term (terms:term-op a-term)
                                                     reduced-args
                                                     (modules:module-ops module)))])
        (rewrite-head-once with-reduced-args module))
      (rewrite-head-once a-term module)))

(define (reduce a-term module)
  (let loop ([a-term a-term])
    (let* ([rewritten-term (rewrite-leftmost-innermost a-term module)])
      (if (eq? rewritten-term a-term)
          a-term
          (loop rewritten-term)))))
