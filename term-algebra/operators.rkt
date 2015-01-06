#lang racket

(provide empty-op-set
         add-op
         (struct-out op))

(require (prefix-in sorts: term-algebra/sorts))

(struct op (symbol domain range properties)
        #:transparent)

; Page 66: To avoid ambiguous expressions we require that if the sorts
; in the arities of two operators with the same syntactic form are
; pairwise in the same connected components, then the sorts in the
; coarities must likewise be in the same connected component.

;
; Management of op-sets
;
; An op-set is stored as a hash mapping symbols to overloaded
; operators. An overloaded operator is represented as a hash
; mapping argument kinds to lists of individual operators.
; These lists are sorted by increasing domain sort, such that
; operator lookup can proceed linearly.
;

(define (empty-op-set)
  (hash))

(define (add-op op sorts op-set)
  (let* ([symbol (op-symbol op)]
         [domain-kinds (for/list ([s (op-domain op)]) (sorts:kind s sorts))]
         [range-kind (sorts:kind (op-range op) sorts)]
         [ops-for-symbol (hash-ref op-set symbol (hash))]
         [ops-for-kinds (hash-ref ops-for-symbol domain-kinds empty)])
    (unless (empty? ops-for-kinds)
      ; Since ops are added one by one, it is sufficient to check
      ; compatibility with the first existing one.
      (let ([opk (first ops-for-kinds)])
        (unless (equal? (op-properties op) (op-properties opk))
          (error (format "Operator ~s must have properties ~s"
                         op (op-properties opk))))
        (unless (equal? range-kind (sorts:kind (op-range opk) sorts))
          (error (format "Operator ~s must have the kind of sort ~s"
                         op (op-range opk))))))
    (define-values (before after)
      (splitf-at ops-for-kinds (λ (opk) (sorts:is-subsort?
                                         (op-range opk) (op-range op) sorts))))
    (hash-set op-set
              symbol (hash-set ops-for-symbol
                               domain-kinds  (append before (cons op after))))))

; Better define
; (struct overloaded-op (domain-kinds) range-kind properties)
