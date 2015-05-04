#lang racket

(provide empty-op-set op-set-sorts
         add-op add-special-op merge-op-set
         has-op? has-special-op? has-var-arity?
         lookup-op lookup-var-arity-op
         op-definitions
         (struct-out signature))

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
; A signature consists of a a domain (a list of sorts) and a range sort
;
(struct signature (domain symmetric? range origin)
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
  (define debug (member (operator-symbol op) '()))
  (when debug (printf "--- preregular? --- ~s\n" domain))
  ; Iterate over all domains made up of subsorts of the current domain
  (for*/and ([arg-sorts (cartesian-product
                         (map (λ (s) (sorts:subsorts s sorts)) domain))])
    (when debug (printf "+ Argument sorts ~s\n" arg-sorts))
    ; Pick the ranges of the operator signatures that match arg-sorts.
    (let ([ranges
           (for*/list ([sig (operator-signatures op)]
                       #:when (andmap (λ (s1 s2) (sorts:is-sort? s1 s2 sorts))
                                      arg-sorts (signature-domain sig)))
             (when debug (printf "~s -> ~s\n"
                                 (signature-domain sig)
                                 (signature-range sig)))
             (signature-range sig))])
      ; The first of these ranges must be the lowest sort in the list.
      (andmap (λ (r) (sorts:is-sort? (first ranges) r sorts))
              (rest ranges)))))

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

(define (add-op symbol domain range properties origin imports ops
                strict-checking)
  (define debug (member symbol '()))
  (when debug
    (printf "--- add-op ~s ~s ---\n" domain range))
  (unless (sorts:has-sort? range (op-set-sorts ops))
    (error "Undefined sort " range))
  (for ([sort domain])
    (unless (sorts:has-sort? sort (op-set-sorts ops))
      (error "Undefined sort " sort)))
  (define var-arity (set-member? properties 'var-arity))
  (when (and var-arity (not (equal? (length domain) 1)))
    (error "Wrong number of domain sorts for variable arity operator " symbol))
  (define any-in-domain (member 'Any domain))
  (let* ([sorts (op-set-sorts ops)]
         [domain-kinds (cond
                        [(and any-in-domain var-arity) 'Any]
                        [any-in-domain (map (λ (s) 'Any) domain)]
                        [var-arity (sorts:kind (first domain) sorts)]
                        [else (map (λ (s) (sorts:kind s sorts)) domain)])]
         [range-kind (sorts:kind range sorts)]
         [ops-for-symbol (hash-ref (op-set-ops ops) symbol (hash))]
         [op (hash-ref ops-for-symbol domain-kinds #f)]
         [sig (signature domain (set-member? properties 'symmetric)
                         range origin)])
    (let ([extended-op
           (if op
               (let* ([op-signatures (operator-signatures op)]
                      [first-signature (first op-signatures)]
                      [first-range (signature-range first-signature)])
                 (when (ormap (λ (s) (hash-ref imports (signature-origin s) #f))
                              op-signatures)
                   (error (format "Operator ~s was imported in restricted mode"
                                  symbol)))
                 (unless (equal? properties (operator-properties op))
                   (error (format "Operator ~s must have properties ~s"
                                  symbol (operator-properties op))))
                 (unless (equal? range-kind (sorts:kind first-range sorts))
                   (error (format "Operator ~s must have the kind of sort ~s"
                                  symbol first-range)))
                 (define-values (before after)
                   (splitf-at op-signatures
                              (λ (sig) (not (sorts:is-subsort?
                                             range (signature-range sig)
                                             sorts)))))
                 (when debug
                   (printf "~s | ~s | ~s\n" before sig after))
                 (if (and (not (empty? after))
                            (equal? (signature-domain sig)
                                    (signature-domain (first after)))
                            (equal? (signature-range sig)
                                    (signature-range (first after))))
                     (if strict-checking
                         (error (format "Signature ~s -> ~s already defined"
                                        (cons symbol (signature-domain
                                                      (first after)))
                                        (signature-range (first after))))
                         (operator symbol op-signatures properties))
                   (operator symbol (append before (cons sig after))
                             properties)))
               (begin
                 (check-for-conflicts domain-kinds (hash-keys ops-for-symbol))
                 (operator symbol (list sig) properties)))])
      (unless (preregular? extended-op sorts domain)
        (error (format "Operator ~s is not preregular after addition of signature ~s -> ~s" symbol domain range)))
      (op-set (op-set-sorts ops)
              (hash-set (op-set-ops ops)
                        symbol (hash-set ops-for-symbol
                                         domain-kinds extended-op))
              (op-set-special-ops ops)))))

(define (add-special-op special-op origin imports ops)
  ; origin and imports aren't used by the current implementation
  (unless (member special-op '(string symbol natural-number
                                      integer-number rational-number))
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
                          (signature-domain sig)
                          (signature-range sig)
                          (operator-properties op)
                          (signature-origin sig))))))))))

(define (merge-op-set to-merge prior-imports ops)
  (define unrestricted (for/hash ([(hashcode _) (in-hash prior-imports)])
                         (values hashcode #f)))
  (define merged-ops
    (for/fold ([ops ops])
              ([spec (op-definitions to-merge)]
               #:when (not (hash-has-key? prior-imports (last spec))))
      (apply add-op (append spec (list unrestricted ops #f)))))
  (op-set (op-set-sorts merged-ops)
          (op-set-ops merged-ops)
          (set-union (op-set-special-ops merged-ops)
                     (op-set-special-ops to-merge))))

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

(define (all-equal? lst)
  (equal? 1 (set-count (list->set lst))))

(define (lookup-op symbol arg-sorts ops)

  (define sorts (op-set-sorts ops))
  (define (is-sort? s1 s2) (sorts:is-sort? s1 s2 sorts))

  (define (lookup-fixed arg-sorts op-fixed)
    (and op-fixed
         (for/first ([sig (operator-signatures op-fixed)]
                     #:when (and (equal? (length arg-sorts)
                                         (length (signature-domain sig)))
                                 (andmap is-sort?
                                         arg-sorts (signature-domain sig))))
           sig)))

  (define (lookup-var-or-any arg-sorts op)
    (and op
         (for/first ([sig (operator-signatures op)]
                     #:when (andmap (λ (s) (is-sort?
                                            s (first (signature-domain sig))))
                                    arg-sorts))
           sig)))

  (define (lookup-var-any op-any)
    ; A var-arity operator with domain sort Any matches everything.
    (and op-any
         (first (operator-signatures op-any))))

  (define (kindsig-fixed domain-kinds op-fixed)
    (and op-fixed
         (let* ([sig (first (operator-signatures op-fixed))])
           (signature domain-kinds
                      (signature-symmetric? sig)
                      (sorts:kind (signature-range sig) sorts)
                      #f))))

  (define (kindsig-var domain-kinds op-var)
    (and op-var
         (all-equal? domain-kinds)
         (let* ([sig (first (operator-signatures op-var))])
           (signature (list (first domain-kinds))
                      (signature-symmetric? sig)
                      (sorts:kind (signature-range sig) sorts)
                      #f))))

  (define (kind-sig domain-kinds op-fixed op-var)
    (and (not (empty? domain-kinds))
         (or (kindsig-fixed domain-kinds op-fixed)
             (kindsig-var domain-kinds op-var))))

  (define (sort-sig arg-sorts op-fixed op-var op-any op-var-any)
    (or (lookup-fixed arg-sorts op-fixed)
        (and (not (empty? arg-sorts))
             (or (lookup-var-or-any arg-sorts op-var)
                 (lookup-var-or-any arg-sorts op-any)
                 (lookup-var-any op-var-any)))))
  
  (let* ([ops-for-symbol (hash-ref (op-set-ops ops) symbol (hash))]
         [domain-kinds (map (λ (s) (sorts:kind s sorts)) arg-sorts)]
         [kind-sig? (ormap sorts:kind? arg-sorts)]
         [op-fixed (hash-ref ops-for-symbol domain-kinds #f)]
         [op-var (and (not (empty? domain-kinds))
                      (hash-ref ops-for-symbol (first domain-kinds) #f))]
         [op-any (hash-ref ops-for-symbol (map (λ (s) 'Any) arg-sorts) #f)]
         [op-var-any (hash-ref ops-for-symbol 'Any #f)])
    (if kind-sig?
        (kind-sig domain-kinds op-fixed op-var)
        (or
         (sort-sig arg-sorts op-fixed op-var op-any op-var-any)
         (kind-sig domain-kinds op-fixed op-var)))))

(define (lookup-var-arity-op symbol arg-sorts ops)

  (define sorts (op-set-sorts ops))
  (define (is-sort? s1 s2) (sorts:is-sort? s1 s2 sorts))

  (define (lookup-var arg-sort op-var)
    (and op-var
         (for/first ([sig (operator-signatures op-var)]
                     #:when (is-sort?
                             arg-sort (first (signature-domain sig))))
           sig)))

  (define (lookup-var-any op-any)
    ; A var-arity operator with domain sort Any matches everything.
    (and op-any
         (signature-range (first (operator-signatures op-any)))))

  (define (kindsig-var domain-kind op-var)
    (and op-var
         (let* ([sig (first (operator-signatures op-var))])
           (signature (list domain-kind)
                      (signature-symmetric? sig)
                      (sorts:kind (signature-range sig) sorts)
                      #f))))

  (and (not (empty? arg-sorts))
       (let* ([kind-sig? (ormap sorts:kind? arg-sorts)]
              [domain-kinds (map (λ (s) (sorts:kind s sorts)) arg-sorts)]
              [domain-kind (first domain-kinds)]
              [ops-for-symbol (hash-ref (op-set-ops ops) symbol (hash))]
              [op-var (hash-ref ops-for-symbol domain-kind #f)])
         (and (all-equal? domain-kinds)
              (if kind-sig?
                  (kindsig-var domain-kind op-var)
                  (or (lookup-var
                       (apply sorts:least-common-sort (cons sorts arg-sorts))
                       op-var)
                      (lookup-var-any (hash-ref ops-for-symbol 'Any #f))
                      (kindsig-var domain-kind op-var)))))))
