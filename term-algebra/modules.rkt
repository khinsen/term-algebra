#lang racket

(provide define-module define-meta-module term meta-term meta)

(require (prefix-in terms: term-algebra/terms)
         (for-syntax syntax/parse)
         (only-in file/sha1 sha1)
         "./condd.rkt")

;
; The module registry
;
(define (hash-of-string s)
  (sha1 (open-input-string s)))

(define (hash-of-meta-module meta-module)
  (let ([o (open-output-string)])
    (write meta-module o)
    (hash-of-string (get-output-string o))))

(define *modules* (make-hash))

(define (register-module module)
  (hash-set! *modules* (terms:module-meta-hash module) (make-weak-box module)))

(define (lookup-module-hash hash)
  (weak-box-value (hash-ref *modules* hash)))

(define (make-module module-name ops meta-terms)
  (let* ([meta-hash (if meta-terms
                        (hash-of-meta-module meta-terms)
                        (hash-of-string (symbol->string module-name)))]
         [mod (terms:module module-name ops meta-terms meta-hash)])
    (register-module mod)
    mod))

;
; The meta-representation of modules as terms
;
(define op-op (terms:op 'op '(name ...) '()))

(define op-vars (terms:op 'vars '(...) '()))
(define op-eqrule (terms:op '=-> '(left right) '()))
(define op-eq (terms:op '== '(x y) '()))
(define op-ceqrule (terms:op '=->? '(left condition right) '()))

(define op-term (terms:op 'term '(symbol ...) '()))

(define op-ops (terms:op 'ops '(op ...) '()))
(define op-rules (terms:op 'rules '(rule ...) '()))
(define op-module (terms:op 'module '(name ops meta) '()))

(define op-imports (terms:op 'imports '(module ...) '()))
(define op-use (terms:op 'use '(name) '()))

(define meta
  (let ([ops (hash 'op op-op
                   'vars op-vars
                   '=-> op-eqrule
                   '=->? op-ceqrule
                   '== op-eq
                   'term op-term
                   'ops op-ops
                   'rules op-rules
                   'module op-module
                   'imports op-imports
                   'use op-use)])
    (make-module 'meta ops #f)))

(define (meta-module module-name imports decls)
  (let ([ops (filter (λ (t) (eq? (terms:term-op t) op-op)) decls)]
        [rules (filter (λ (t) (member (terms:term-op t) (list op-eqrule
                                                              op-ceqrule)))
                       decls)])
    (terms:term op-module (list module-name
                                (terms:term op-imports imports)
                                (terms:term op-ops ops)
                                (terms:term op-rules rules)))))

;
; Make a "compiled" module from a meta-module
;
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
    (let ([op (hash-ref ops op-or-var)]
          [arg-terms (for/list ([arg args]) (term-from-meta ops vars arg))])
      (if (equal? (length arg-terms) (length (terms:op-args op)))
          (terms:term op arg-terms)
          (error "wrong number of arguments for op " op)))]
   [(hash-has-key? vars op-or-var)
    (if (empty? args)
        (hash-ref vars op-or-var)
        (error "var has non-empty argument list: " args))]
   [else (error "undefined op or var: " op-or-var)]))

(define (module-from-meta module-term)

  (define (do-import import-term ops)
    (let* ([import-op (terms:term-op import-term)]
           [import-hash (first (terms:term-args import-term))]
           [imported-module (lookup-module-hash import-hash)]
           [imported-ops (hash-values (terms:module-ops imported-module))])
      (cond
       [(equal? import-op op-use)
        (for/fold ([ops ops])
                  ([op imported-ops])
          (let ([symbol (terms:op-symbol op)])
            (if (hash-has-key? ops symbol)
                (error "op already defined: " symbol)
                (hash-set ops symbol op))))]
       [else
        (error "unknown import op " import-op)])))

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
    (error "not a meta-module: " module-term)]
   #:do (match-define (list module-name imports-term ops-term rules-term) 
                      (terms:term-args module-term))
   [(not (and (terms:term? imports-term)
              (equal? (terms:term-op imports-term) op-imports)))
    (error "not an imports term: " imports-term)]
   [(not (and (terms:term? ops-term)
              (equal? (terms:term-op ops-term) op-ops)))
    (error "not an ops module: " ops-term)]
   [(not (and (terms:term? rules-term)
              (equal? (terms:term-op rules-term) op-rules)))
    (error "not an ops module: " ops-term)]
   #:do (define ops (hash))
   #:do (set! ops (foldl do-import ops (terms:term-args imports-term)))
   #:do (set! ops (foldl add-op ops (terms:term-args ops-term)))
   #:do (set! ops (foldl add-rule ops (terms:term-args rules-term)))
   [else (make-module module-name ops module-term)]))

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

  (define-syntax-class decl
    #:description "declaration"
    #:datum-literals (define-op =->)

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
    (pattern (=-> left:term  #:if cond:term right:term)
             #:with value #'(terms:term op-ceqrule
                                        (list (terms:term op-vars '())
                                              left.value
                                              cond.value
                                              right.value)))
    (pattern (=-> #:vars (var:id ...) left:term  #:if cond:term right:term)
             #:with value #'(terms:term op-ceqrule
                                        (list (terms:term op-vars
                                                          (list (quote var) ...))
                                              left.value
                                              cond.value
                                              right.value)))
    (pattern (=-> #:var var:id left:term  #:if cond:term right:term)
             #:with value #'(terms:term op-ceqrule
                                        (list (terms:term op-vars
                                                          (list (quote var)))
                                              left.value
                                              cond.value
                                              right.value))))
  
  (define-syntax-class import
    #:description "import"
    #:datum-literals (use)
    (pattern (use module:id)
             #:with value #'(terms:term
                             op-use (list (terms:module-meta-hash module))))))

(define-syntax (define-meta-module stx)
  (syntax-parse stx
    [(_ module-name:id
        import:import ...
        decl:decl ...)
     #'(define module-name
         (meta-module (quote module-name)
                      (list import.value ...)
                      (list decl.value ...)))]))

(define-syntax (define-module stx)
  (syntax-parse stx
    [(_ module-name:id
        import:import ...
        decl:decl ...)
     #'(define module-name
         (module-from-meta (meta-module (quote module-name)
                      (list import.value ...)
                      (list decl.value ...))))]))

(define-syntax (meta-term stx)
  (syntax-parse stx
    [(_  expr:term)
     #'expr.value]))

(define-syntax (term stx)
  (syntax-parse stx
    [(_ module:expr expr:term)
     #'(term-from-meta (terms:module-ops module) (hash) expr.value)]))
