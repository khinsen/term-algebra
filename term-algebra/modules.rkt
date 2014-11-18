#lang racket

; A plain macro version of the module definition - no #lang

(require (prefix-in terms: term-algebra/terms)
         (for-syntax syntax/parse)
         "./condd.rkt")

;
; The external representation of modules as terms
;
(define op-op (terms:op 'op '(name ...) '()))
(define op-eqrule (terms:op '=-> '(left right) '()))
(define op-ceqrule (terms:op '=->? '(left condition right) '()))
(define op-term (terms:op 'term '(symbol ...) '()))
;; (define op-id (terms:op 'id '(symbol) '()))
;; (define op-arg (terms:op 'arg '(term) '()))

(define op-ops (terms:op 'ops '(op ...) '()))
(define op-rules (terms:op 'rules '(rule ...) '()))
(define op-module (terms:op 'module '(ops rules) '()))

(define (terms-module . decls)
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

(define (term-from-meta ops term-term)
  (condd
   [(not (and (terms:term? term-term)
              (equal? (terms:term-op term-term) op-term)))
    (error "not a term: " term-term)]
   #:do (match-define (cons op args) (terms:term-args term-term))
   [(not (symbol? op))
    (error "not a symbol: " op)]
   [(not (hash-has-key? ops op))
    (error "undefined op: " op)]
   [else (terms:term (hash-ref ops op)
                     (for/list ([arg args]) (term-from-meta ops arg)))]))

(define (module-from-meta module-term)

  (define (add-op op-term ops)
    (let* ([op (op-from-meta op-term)]
           [symbol (terms:op-symbol op)])
      (if (hash-has-key? ops symbol)
          (error "op already defined: " symbol)
          (hash-set ops symbol op))))

  (define (add-rule rule-term ops)
    (unless (and (terms:term? rule-term)
                 (member (terms:term-op rule-term) (list op-eqrule)))
      (error "not a rule: " rule-term))
    (match-let* ([(list left right) (terms:term-args rule-term)]
                 [left-term (term-from-meta ops left)]
                 [right-term (term-from-meta ops right)]
                 [left-op (terms:term-op left-term)])
      (terms:set-op-rules! left-op
                           (append (terms:op-rules left-op)
                                   (list (cons left-term right-term)))))
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
                                        (list (quote op) args.value ...)))))

(define-syntax (define-module stx)
  
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
                                        (list left.value right.value)))
    (pattern (=-> left:term right:term #:if cond:term)
             #:with value #'(terms:term op-ceqrule
                                        (list left.value
                                              cond.value
                                              right.value)))
    )
  
  (syntax-parse stx
    [(_ module-name:id
        decl:decl
        ...)
     #'(define module-name
         (module-from-meta (terms-module decl.value ...)))]))

(define-syntax (meta-term stx)
  (syntax-parse stx
    [(_  expr:term)
     #'expr.value]))

(define-syntax (term stx)
  (syntax-parse stx
    [(_ module:expr expr:term)
     #'(term-from-meta (module-ops module) expr.value)]))


; Test code

(define-module truth
  (define-op true)
  (define-op false))

(define-module boolean

  (define-op true)
  (define-op false)
 
  (define-op (not x))
  (=-> (not true) false)
  (=-> (not false) true)
  
  )

(terms:reduce (term boolean (not true)))
