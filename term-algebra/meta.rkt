#lang racket

(provide n-term n-pattern n-node
         meta-up meta-down
         make-vterm (struct-out vterm) (struct-out node-vterm)
         reduce-vterm in-vterm-reduction
         vterm-equal?
         check-node node-hashcode)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in terms: term-algebra/terms)
         (prefix-in nodes: term-algebra/nodes)
         (prefix-in builtin: term-algebra/builtin)
         (prefix-in rules: term-algebra/rules)
         (prefix-in rewrite: term-algebra/rewrite)
         (for-syntax syntax/parse)
         racket/generator)

;
; The data structure for a term validated with reference to a node
;
(struct vterm (node sort term)
        #:transparent
        #:property prop:custom-write
        (lambda (vterm port mode)
          (define (write-term term op-set port)
            (cond
              [(terms:term? term)
               (let ([op (terms:term-op term)])
                 (if (and (null? (terms:term-args term))
                          (not (operators:has-var-arity? op op-set)))
                     (write op port)
                     (begin
                       (write-string "(" port)
                       (write op port)
                       (for ([arg (terms:term-args term)])
                         (write-string " " port)
                         (write-term arg op-set port))
                       (write-string ")" port))))]
              [else
               (print term port)]))
          (let* ([mod (vterm-node vterm)]
                 [ops (nodes:node-ops mod)]
                 [term (vterm-term vterm)])
            (write (nodes:node-name mod) port)
            (write-string ":" port)
            (write (vterm-sort vterm) port)
            (write-string ":" port)
            (write-term term ops port))))

; A specialized version for node terms that caches the
; internal representation
(struct node-vterm vterm ([internal #:mutable]))

;
; The meta-representation of terms and nodes as terms
;
(nodes:define-builtin-node n-term

  (use builtin:rational)
  (use builtin:string)
  (use builtin:symbol)

  (sorts Term ArgList)
  (subsorts [Rational Term] [String Term] [Symbol Term])

  (op (term Symbol ArgList) Term)
  (op (args Term ...) ArgList)
  (op (args) ArgList))

(nodes:define-builtin-node n-pattern

  (include n-term)

  (sorts Pattern PatternArgList)
  (subsorts [Term Pattern] [ArgList PatternArgList])

  (op (var-ref Symbol) Pattern)
  (op (pattern Symbol PatternArgList) Pattern)
  (op (args Pattern ...) PatternArgList))

(nodes:define-builtin-node n-node

  (include n-pattern)
  (use builtin:truth)

  (sorts Node
         ImportList Import
         SortList SubsortList Subsort
         OpList Op
         Domain EmptyDomain VarLengthDomain
         RuleList Rule VarList Var)
  (subsorts [EmptyDomain Domain]
            [VarLengthDomain Domain])

  (op (builtin-node Symbol) Node)

  (op (node Symbol ImportList SortList SubsortList OpList RuleList) Node)

  (op (imports Import ...) ImportList)
  (op (imports) ImportList)
  (op (use String) Import)
  (op (use Node) Import)
  (op (include String) Import)
  (op (include Node) Import)

  (op (sorts Symbol ...) SortList)
  (op (sorts) SortList)

  (op (subsorts Subsort ...) SubsortList)
  (op (subsorts) SubsortList)
  (op (subsort Symbol Symbol) Subsort)

  (op (ops Op ...) OpList)
  (op (ops) OpList)
  (op (op Symbol Domain Symbol) Op)
  (op (symop Symbol VarLengthDomain Symbol) Op)
  (op (symop Symbol EmptyDomain Symbol) Op)
  (op (domain Symbol ...) Domain)
  (op (domain) EmptyDomain)
  (op (vl-domain Symbol) VarLengthDomain)

  (op (rules Rule ...) RuleList)
  (op (rules) RuleList)
  (op (=> VarList Pattern Pattern Pattern) Rule)
  (op (vars Var ...) VarList)
  (op (vars) VarList)
  (op (var Symbol Symbol) Var)
  (op (svar Symbol Symbol Boolean) Var)
  (op no-condition Pattern))

(define n-term-ops (nodes:node-ops n-term))
(define n-node-ops (nodes:node-ops n-node))

(define (make-vterm node term)
  (if (and (nodes:imports? node n-node)
           (member (terms:term-op term) '(node builtin-node)))
      (node-vterm node (terms:sort-of term) term #f)
      (vterm node (terms:sort-of term) term)))

(define (reduce-vterm vterm)
  (let ([mod (vterm-node vterm)]
        [term (vterm-term vterm)])
    (make-vterm mod (rewrite:reduce term mod))))

(define (in-vterm-reduction vterm)
  (in-generator
   (let ([mod (vterm-node vterm)]
         [term (vterm-term vterm)])
     (for ([r (rewrite:in-reduction term mod)])
       (yield (make-vterm mod r))))))

(define (vterm-equal? vterm1 vterm2)
  (and (eq? (vterm-node vterm1) (vterm-node vterm2))
       (terms:term-equal? (vterm-term vterm1) (vterm-term vterm2))))

; used in test-nodes.rkt
(define (internal-node m-vterm)
  (unless (node-vterm? m-vterm)
    (error "Not a node: " m-vterm))
  (node-vterm-internal m-vterm))

(define (meta-up* a-term)
  (cond
    [(terms:term? a-term)
     (terms:make-term 'term
                      (list (terms:term-op a-term)
                            (terms:make-term 'args
                                             (map meta-up*
                                                  (terms:term-args a-term))
                                             n-term-ops))
                      n-term-ops)]
    [else a-term]))

(define (meta-up vterm-or-node)
  (make-vterm n-term (meta-up* (vterm-term vterm-or-node))))

;
; Parse meta-terms into concrete terms
;
(define-match-expander mterm
  (lambda (stx)
    (syntax-parse stx
      [(_ an-op some-args)
       #'(struct* terms:term ([op (==  an-op equal?)] [args some-args]))])))

(define-match-expander mterm0
  (lambda (stx)
    (syntax-parse stx
      [(_ an-op)
       #'(struct* terms:term ([op (==  an-op equal?)] [args (list)]))])))

(define (term-from-meta node op-symbol args)
  (terms:make-term op-symbol (map (λ (arg) (meta-down* node arg)) args)
                   (nodes:node-ops node)))

(define (node-from-meta meta-terms strict-checking)

  (define (subsort-list subsort-terms)
    (for/list ([ss subsort-terms])
      (match ss
        [(mterm 'subsort (list s1 s2))
         (cons s1 s2)]
        [_ (error "Invalid subsort term " ss)])))

  (define (op-list op-terms)
    (for/list ([op op-terms])
      (match op
        [(mterm 'op (list name (mterm 'domain sort-symbols) range))
         (list name sort-symbols range (set))]
        [(mterm 'op (list name (mterm 'vl-domain sort-symbols) range))
         (list name sort-symbols range (set 'var-arity))]
        [(mterm 'symop (list name (mterm0 'domain) range))
         (list name empty range (set 'symmetric))]
        [(mterm 'symop (list name (mterm 'vl-domain sort-symbols) range))
         (list name sort-symbols range (set 'var-arity 'symmetric))]
        [_ (error "Invalid op term " op)])))

  (define (import-list import-terms)

    (define (internal-node node-or-hashcode)
      (cond
        [(string? node-or-hashcode)
         (nodes:lookup-node-hash node-or-hashcode)]
        [(and (terms:term? node-or-hashcode)
              (equal? (terms:sort-of node-or-hashcode) 'Node))
         (node-from-meta node-or-hashcode strict-checking)]
        [else (error "Not a valid node " node-or-hashcode)]))

    (for/list ([import import-terms])
      (match import
        [(mterm 'use (list node-or-hashcode))
         (cons (internal-node node-or-hashcode) #t)]
        [(mterm 'include (list node-or-hashcode))
         (cons (internal-node node-or-hashcode) #f)]
        [_ (error "Invalid import term " import)])))

  (define (rule-list node rule-terms)

    (define (var-hash var-terms)
      (for/fold ([vars (hash)])
                ([var-term var-terms])
        (match var-term
          [(mterm 'var (list var sort))
           (hash-set vars var (cons sort 'one))]
          [(mterm 'svar (list var sort allow-zero?))
           (hash-set vars var (cons sort
                                    (if (equal? (terms:term-op allow-zero?)
                                                'true)
                                        'zero-or-more
                                        'one-or-more)))]
          [_ (error "Invalid var term " var-term)])))

    (define (pattern-from-meta node vars meta-term)
      (match meta-term
        [(or (mterm 'term (list op (mterm 'args args)))
             (mterm 'pattern (list op (mterm 'args args))))
         (let* ([args (map (λ (arg)
                             (pattern-from-meta node vars arg)) args)])
           (terms:make-pattern op args (nodes:node-ops node)))]
        [(mterm 'var-ref (list var-name))
         (let ([var-spec (hash-ref vars var-name)])
           (case (cdr var-spec)
             ['one (terms:var var-name (car var-spec))]
             ['zero-or-more (terms:svar var-name (car var-spec) #t)]
             ['one-or-more  (terms:svar var-name (car var-spec) #f)]))]
        [(mterm 'no-condition empty)
         #f]
        [_ (terms:make-special-term meta-term (nodes:node-ops node))]))

    (for/list ([rule rule-terms])
      (match rule
        [(mterm '=> (list (mterm 'vars vars) pattern condition replacement))
         (let* ([vars (var-hash vars)]
                [rule-pattern (pattern-from-meta node vars pattern)]
                [rule-condition (pattern-from-meta node vars condition)]
                [rule-replacement (pattern-from-meta node vars replacement)])
           (rules:make-rule (nodes:node-ops node)
                            vars rule-pattern rule-condition rule-replacement))]
        [_ (error "Invalid rule term " rule)])))

  (match meta-terms
    [(mterm 'node (list name
                          (mterm 'imports import-terms)
                          (mterm 'sorts sort-symbols)
                          (mterm 'subsorts subsort-terms)
                          (mterm 'ops operator-terms)
                          (mterm 'rules rule-terms)))
     (let* ([mod (nodes:make-node name meta-terms
                                      (import-list import-terms)
                                      sort-symbols (subsort-list subsort-terms)
                                      (op-list operator-terms)
                                      empty empty strict-checking)]
            [rules (nodes:node-rules mod)]
            [hashcode (nodes:node-hashcode mod)])
       (for ([rule (rule-list mod rule-terms)])
         (let ([pattern (rules:rule-pattern rule)]
               [imports (nodes:node-imports mod)])
           (when (and (terms:term? pattern)
                      (hash-ref imports 
                                (terms:op-origin pattern)
                                #f))
             (error (format "Cannot add rule for operator ~s imported in restricted mode"
                            (terms:term-op pattern)))))
         (rules:add-rule! rule hashcode  rules))
       mod)]
    [(mterm 'builtin-node (list name))
     (or (builtin:lookup-node name)
         (case name
           ['term n-term]
           ['pattern n-pattern]
           ['node n-node]
           [else (error "Unknown builtin node: " name)]))]
    [_ (error "Invalid meta node " meta-terms)]))

(define (meta-down* node a-term)
  (match a-term
    [(mterm 'term (list op-symbol (mterm 'args args)))
     (term-from-meta node op-symbol args)]
    [(terms:term _ _ _)
     (error "Invalid meta-term " a-term)]
    [_ (terms:make-special-term a-term (nodes:node-ops node))]))

(define (meta-down node a-term)
  (unless (node-vterm? node)
    (error (format "Not a node: ~s" node)))
  (unless (vterm? a-term)
    (error (format "Not a term: ~s" a-term)))
  (let ([int-mod (node-vterm-internal (compile-node node #f))])
    (make-vterm int-mod (meta-down* int-mod (vterm-term a-term)))))

(define (compile-node node-term strict-checking)
  (unless (node-vterm? node-term)
    (error (format "Not a node: ~s" node-term)))
  (unless (node-vterm-internal node-term)
    (set-node-vterm-internal! node-term
                                (node-from-meta (vterm-term node-term)
                                                  strict-checking)))
  node-term)

(define (node-hashcode node)
  (nodes:node-hashcode
   (if (node-vterm? node)
       (node-vterm-internal (compile-node node #f))
       node)))

(define (check-node node-term)
  (compile-node node-term #t))
