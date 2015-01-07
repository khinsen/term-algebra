#lang racket

(provide empty-op-set
         add-op
         (struct-out operator))

(require (prefix-in sorts: term-algebra/sorts)
         racket/generator)

(struct operator (symbol signatures properties)
        #:transparent)

;
; Management of op-sets
;
; An op-set is stored as a hash mapping symbols to overloaded
; operators. An overloaded operator (struct operator) has a single
; symbol and a single set of properties plus a list of signatures.
; Each signature entry is a pair of a domain (a list of sorts)
; and a range (a single sort). Signatures are sorted by increasing
; range sort, such that operator lookup can proceed linearly.
;

(define (empty-op-set)
  (hash))

(define (cartesian-product seqs)
  (in-generator
   (if (empty? seqs)
       (yield empty)
       (for* ([s (first seqs)]
              [ss (cartesian-product (rest seqs))])
         (yield (cons s ss))))))

(define (preregular? op sorts domain)
  (let ([ranges
         (for*/list ([arg-sorts (cartesian-product
                                 (map (λ (s) (sorts:subsorts s sorts)) domain))]
                     [sig (operator-signatures op)]
                     #:when (andmap (λ (s1 s2) (sorts:is-sort? s1 s2 sorts))
                                    arg-sorts (car sig)))
           (cdr sig))])
    (andmap (λ (r) (sorts:is-sort? (first ranges) r sorts)) (rest ranges))))

(define (add-op symbol domain range properties sorts op-set)
  (let* ([domain-kinds (map (λ (s) (sorts:kind s sorts)) domain)]
         [range-kind (sorts:kind range sorts)]
         [ops-for-symbol (hash-ref op-set symbol (hash))]
         [op (hash-ref ops-for-symbol domain-kinds #f)])
    (let ([extended-op
           (if op
               (let* ([op-signatures (operator-signatures op)]
                      [first-signature (first op-signatures)]
                      [first-range (cdr first-signature)])
                 (unless (equal? properties (operator-properties op))
                   (error (format "Operator ~s must have properties ~s"
                                  symbol (operator-properties op))))
                 (unless (equal? range-kind (sorts:kind first-range sorts))
                   (error (format "Operator ~s must have the kind of sort ~s"
                                  symbol first-range)))
                 (define-values (before after)
                   (splitf-at op-signatures
                              (λ (sig) (sorts:is-subsort?
                                        (cdr sig) range sorts))))
                 (operator symbol (append before
                                          (cons (cons domain range) after))
                           properties))
               (operator symbol (list (cons domain range)) properties))])
      (unless (preregular? extended-op sorts domain)
        (error (format "Operator ~s is not preregular after addition of signature ~s -> ~s" symbol domain range)))
      (hash-set op-set
                symbol (hash-set ops-for-symbol
                                 domain-kinds extended-op)))))
