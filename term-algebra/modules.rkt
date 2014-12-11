#lang racket

(provide define-module define-meta-module term meta-term)

(require (prefix-in terms: term-algebra/terms)
         (for-syntax syntax/parse)
         "./condd.rkt")

;
; The external representation of modules as terms
;
(define op-op (terms:op 'op '(name ...) '()))

(define op-vars (terms:op 'vars '(...) '()))
(define op-eqrule (terms:op '=-> '(left right) '()))
(define op-eq (terms:op '== '(x y) '()))
(define op-ceqrule (terms:op '=->? '(left condition right) '()))

(define op-term (terms:op 'term '(symbol ...) '()))

(define op-ops (terms:op 'ops '(op ...) '()))
(define op-rules (terms:op 'rules '(rule ...) '()))
(define op-module (terms:op 'module '(ops rules) '()))

(define (meta-module . decls)
  (let ([ops (filter (λ (t) (eq? (terms:term-op t) op-op)) decls)]
        [rules (filter (λ (t) (member (terms:term-op t) (list op-eqrule
                                                              op-ceqrule)))
                       decls)])
    (terms:term op-module (list (terms:term op-ops ops)
                                (terms:term op-rules rules)))))

;
; The internal representation of a module
;
(struct module (term ops)
        #:transparent)

(define (op-from-meta op-term)
  (condd
   [(not (and (terms:term? op-term)
              (equal? (terms:term-op op-term) op-op)))
    (error "not an op term: " op-term)]
   #:do (match-define (cons id-symbol arg-symbols) 
                      (terms:term-args op-term))
   [(not (symbol? id-symbol))
    (error "not a symbol: " id-symbol)]
   [(not (and (list? arg-symbols)
              (andmap symbol? arg-symbols)))
    (error "not a list of symbols: " arg-symbols)]
   [else (terms:op id-symbol arg-symbols '())]))

(define (term-from-meta ops vars term-term)
  (condd
   [(not (and (terms:term? term-term)
              (equal? (terms:term-op term-term) op-term)))
    (error "not a term: " term-term)]
   #:do (match-define (cons op-or-var args) (terms:term-args term-term))
   [(not (symbol? op-or-var))
    (error "not a symbol: " op-or-var)]
   [(hash-has-key? ops op-or-var)
    (terms:term (hash-ref ops op-or-var)
                (for/list ([arg args]) (term-from-meta ops vars arg)))]
   [(hash-has-key? vars op-or-var)
    (hash-ref vars op-or-var)]
   [else (error "undefined op or var: " op-or-var)]))

(define (module-from-meta module-term)

  (define (add-op op-term ops)
    (let* ([op (op-from-meta op-term)]
           [symbol (terms:op-symbol op)])
      (if (hash-has-key? ops symbol)
          (error "op already defined: " symbol)
          (hash-set ops symbol op))))

  (define (add-var var-symbol vars)
    (if (hash-has-key? vars var-symbol)
        (error "var already defined: " var-symbol)
        (hash-set vars var-symbol (terms:var var-symbol))))

  (define (add-rule rule-term ops)
    (unless (and (terms:term? rule-term)
                 (member (terms:term-op rule-term) (list op-eqrule op-ceqrule)))
      (error "not a rule: " rule-term))
    (if (equal? (terms:term-op rule-term) op-eqrule)
        (match-let* ([(list vars left right) (terms:term-args rule-term)]
                     [vars (foldl add-var (hash) (terms:term-args vars))]
                     [left-term (term-from-meta ops vars left)]
                     [right-term (term-from-meta ops vars right)]
                     [left-op (terms:term-op left-term)])
          (terms:set-op-rules! left-op
                               (append (terms:op-rules left-op)
                                       (list (cons left-term right-term)))))
        (match-let* ([(list vars left c right) (terms:term-args rule-term)]
                     [vars (foldl add-var (hash) (terms:term-args vars))]
                     [cond-term (term-from-meta ops vars c)]
                     [left-term (term-from-meta ops vars left)]
                     [right-term (term-from-meta ops vars right)]
                     [left-op (terms:term-op left-term)])
          (terms:set-op-rules! left-op
                               (append (terms:op-rules left-op)
                                       (list (cons left-term
                                                   (cons cond-term
                                                         right-term)))))))
    ops)

  (condd
   [(not (and (terms:term? module-term)
              (equal? (terms:term-op module-term) op-module)))
    (error "not a term module: " module-term)]
   #:do (match-define (list ops-term rules-term) 
                      (terms:term-args module-term))
   #:do (define ops (foldl add-op (hash) (terms:term-args ops-term)))
   #:do (define ops-with-rules (foldl add-rule ops (terms:term-args rules-term)))
   [else (module module-term ops-with-rules)]))

;
; Macros
;
(begin-for-syntax

  (define-syntax-class term
    #:description "term"
    (pattern symbol:id
             #:with value #'(terms:term op-term
                                        (list (quote symbol))))
    (pattern (op:id args:term ...)
             #:with value #'(terms:term op-term
                                        (list (quote op) args.value ...))))

  (define-syntax-class condition
    ; FIXME
    #:description "condition"
    (pattern (== term1:term term2:term)
             #:with value #'(terms:term op-eq
                                        (list term1.value term2.value))))
  
  (define-syntax-class decl
    #:description "declaration"

    (pattern (define-op op-name:id)
             #:with value #'(terms:term op-op (list (quote op-name))))
    (pattern (define-op (op-name:id arg-name:id ...))
             #:with value #'(terms:term op-op
                                        (list (quote op-name)
                                              (quote arg-name) ...)))

    (pattern (=-> left:term right:term)
             #:with value #'(terms:term op-eqrule
                                        (list (terms:term op-vars '())
                                              left.value right.value)))
    (pattern (=-> #:vars (var:id ...) left:term right:term)
             #:with value #'(terms:term op-eqrule
                                        (list (terms:term op-vars
                                                          (list (quote var) ...))
                                              left.value right.value)))
    (pattern (=-> #:var var:id left:term right:term)
             #:with value #'(terms:term op-eqrule
                                        (list (terms:term op-vars
                                                          (list (quote var)))
                                              left.value right.value)))
    (pattern (=-> left:term  #:if cond:condition right:term)
             #:with value #'(terms:term op-ceqrule
                                        (list (terms:term op-vars '())
                                              left.value
                                              cond.value
                                              right.value)))
    (pattern (=-> #:vars (var:id ...) left:term  #:if cond:condition right:term)
             #:with value #'(terms:term op-ceqrule
                                        (list (terms:term op-vars
                                                          (list (quote var) ...))
                                              left.value
                                              cond.value
                                              right.value)))
    (pattern (=-> #:var var:id left:term  #:if cond:condition right:term)
             #:with value #'(terms:term op-ceqrule
                                        (list (terms:term op-vars
                                                          (list (quote var)))
                                              left.value
                                              cond.value
                                              right.value)))))

(define-syntax (define-meta-module stx)
  (syntax-parse stx
    [(_ module-name:id
        decl:decl
        ...)
     #'(define module-name
         (meta-module decl.value ...))]))

(define-syntax (define-module stx)
  (syntax-parse stx
    [(_ module-name:id
        decl:decl
        ...)
     #'(define module-name
         (module-from-meta (meta-module decl.value ...)))]))

(define-syntax (meta-term stx)
  (syntax-parse stx
    [(_  expr:term)
     #'expr.value]))

(define-syntax (term stx)
  (syntax-parse stx
    [(_ module:expr expr:term)
     #'(term-from-meta (module-ops module) (hash) expr.value)]))
