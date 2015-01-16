#lang racket

(provide empty-op-set op-set-sorts
         add-op add-special-op merge-op-set
         has-op? has-special-op? has-var-arity?
         lookup-op op-definitions)

(require (prefix-in sorts: term-algebra/sorts)
         racket/generator)

;
; An overloaded operator has a single symbol and a single set of
; properties plus a list of signatures.  Each signature entry is a
; pair of a domain (a list of sorts) and a range (a single
; sort). Signatures are sorted by increasing range sort, such that
; operator lookup can proceed linearly.
;
(struct operator (symbol signatures properties)
        #:transparent)

;
; Management of op-sets
;
; An op-set consists of a reference to a sort graph and a hash mapping
; symbols to overloaded operators.
;
(struct op-set (sorts ops special-ops)
        #:transparent)

(define (empty-op-set sorts)
  (op-set sorts (hash) (set)))

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

(define (check-for-conflicts domain-kinds prior-domain-kinds)

  (define (conflict? vdk fdk)
    (and (not (empty? fdk))
         (equal? vdk (first fdk))))

  (define-values [fixed variable] (partition list? prior-domain-kinds))
  (when (if (list? domain-kinds)
            ; fixed arity, check for conflicts with var-arities
            (ormap (λ (dk) (conflict? dk domain-kinds)) variable)
            ; var-arity, check for conflicts with fixed arities
            (ormap (λ (dk) (conflict? domain-kinds dk)) fixed))
    (error "Conflicting fixed and variable arity definitions.")))

(define (add-op symbol domain range properties ops)
  (define var-arity (set-member? properties 'var-arity))
  (when (and var-arity (not (equal? (length domain) 1)))
    (error "Wrong number of domain sorts for variable arity operator " symbol))
  (let* ([ops (if var-arity
                  ; Every var-arity operator can also be used as a
                  ; nullary operator.
                  (add-op symbol empty range
                          (set-remove properties 'var-arity)
                          ops)
                  ops)]
         [sorts (op-set-sorts ops)]
         [domain-kinds (if var-arity
                           (sorts:kind (first domain) sorts)
                           (map (λ (s) (sorts:kind s sorts)) domain))]
         [range-kind (sorts:kind range sorts)]
         [ops-for-symbol (hash-ref (op-set-ops ops) symbol (hash))]
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
               (begin
                 (check-for-conflicts domain-kinds (hash-keys ops-for-symbol))
                 (operator symbol (list (cons domain range)) properties)))])
      (unless (preregular? extended-op sorts domain)
        (error (format "Operator ~s is not preregular after addition of signature ~s -> ~s" symbol domain range)))
      (op-set (op-set-sorts ops)
              (hash-set (op-set-ops ops)
                        symbol (hash-set ops-for-symbol
                                         domain-kinds extended-op))
              (op-set-special-ops ops)))))

(define (add-special-op special-op ops)
  (unless (member special-op '(string symbol rational-number))
    (error "Illegial special op " special-op))
  (op-set (op-set-sorts ops)
          (op-set-ops ops)
          (set-add (op-set-special-ops ops) special-op)))

(define (op-definitions ops)
  (in-generator
   (hash-for-each (op-set-ops ops)
     (λ (symbol kind-hash)
       (hash-for-each kind-hash
         (λ (domain-kinds op)
           (for ([sig (operator-signatures op)])
             (yield (list (operator-symbol op)
                          (car sig)
                          (cdr sig)
                          (operator-properties op))))))))))

(define (merge-op-set to-merge mark-imported? ops)
  (let ([merged-ops
         (for/fold ([ops ops])
                   ([spec (op-definitions to-merge)])
           (apply add-op (append spec (list ops))))])
    (op-set (op-set-sorts merged-ops)
            (op-set-ops merged-ops)
            (set-union (op-set-special-ops merged-ops)
                       (op-set-special-ops to-merge)))))

(define (has-op? symbol ops)
  (not (not (hash-ref (op-set-ops ops) symbol #f))))

(define (has-special-op? symbol ops)
  (set-member? (op-set-special-ops ops) symbol))

(define (has-var-arity? symbol ops)
  (let* ([ops-for-symbol (hash-ref (op-set-ops ops) symbol)]
         [all-ops (hash-values ops-for-symbol)])
    (for/first ([op all-ops]
                #:when (set-member? (operator-properties op) 'var-arity))
      op)))

(define (lookup-op symbol arg-sorts ops)

  (define (lookup-fixed arg-sorts op-fixed sorts)
    (if op-fixed
        (for/first ([sig (operator-signatures op-fixed)]
                    #:when (and (equal? (length arg-sorts)
                                        (length (car sig)))
                                (andmap (λ (s1 s2) (sorts:is-sort? s1 s2 sorts))
                                        arg-sorts (car sig))))
          (cdr sig))
        #f))

  (define (lookup-var arg-sorts op-var sorts)
    (if op-var
        (for/first ([sig (operator-signatures op-var)]
                    #:when (andmap (λ (s) (sorts:is-sort?
                                           s (first (car sig)) sorts))
                                   arg-sorts))
          (cdr sig))
        #f))

  (let* ([sorts (op-set-sorts ops)]
         [ops-for-symbol (hash-ref (op-set-ops ops) symbol (hash))]
         [domain-kinds (map (λ (s) (sorts:kind s sorts)) arg-sorts)]
         [op-fixed (hash-ref ops-for-symbol domain-kinds #f)])
    (let ([range (lookup-fixed arg-sorts op-fixed sorts)])
      (or range
          (if (empty? arg-sorts)
              #f
              (let* ([domain-kind (first domain-kinds)]
                     [op-var (hash-ref ops-for-symbol domain-kind #f)])
                (lookup-var arg-sorts op-var sorts)))))))
