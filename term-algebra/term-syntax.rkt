#lang racket

(provide term meta-term pattern n-term n-pattern n-free-pattern)

(require (prefix-in nodes: term-algebra/nodes)
         (prefix-in terms: term-algebra/terms)
         (prefix-in meta: term-algebra/meta)
         (for-syntax syntax/parse))

(define (tterm op args)
  (terms:make-term op args (nodes:node-ops meta:n-term)))

(define (unwrap-vterm vterm)
  (cond
    [(meta:vterm? vterm)
     (meta:vterm-term (meta:meta-up vterm))]
    [(or (symbol? vterm)
         (string? vterm)
         (number? vterm))
     vterm]
    [else (error "illegal term " vterm)]))

(define n-term
  (meta:check-node
   (meta:make-vterm meta:n-node
                    (terms:make-term 'builtin-node (list 'term)
                                     (nodes:node-ops meta:n-node)))))
(define n-pattern
  (meta:check-node
   (meta:make-vterm meta:n-node
                    (terms:make-term 'builtin-node (list 'pattern)
                                     (nodes:node-ops meta:n-node)))))
(define n-free-pattern
  (meta:check-node
   (meta:make-vterm meta:n-node
                    (terms:make-term 'builtin-node (list 'free-pattern)
                                     (nodes:node-ops meta:n-node)))))

(begin-for-syntax
  
    (define-syntax-class atom
    #:description "atomic term"
    #:attributes (value)
    (pattern s:str #:with value #'s)
    (pattern ((~literal quote) symbol:id)
             #:with value #'(quote symbol))
    (pattern x:number #:when (exact? (syntax-e #'x))
             #:with value #'x))

  (define-syntax-class term
    #:description "term"
    #:attributes (value)
    (pattern a:atom #:with value #'a.value)
    (pattern ((~literal unquote) expr:expr)
             #:with value #'(unwrap-vterm expr))
    (pattern symbol:id
             #:with value
             #`(tterm 'term (list (quote symbol)
                                  (tterm 'args (list)))))
    (pattern (symbol:id arg-terms:term+ ...)
             #:with value
             #'(tterm 'term (list (quote symbol)
                                  (tterm 'args (append arg-terms.terms ...))))))
  
  (define-syntax-class term+
    #:description "term or term sequence"
    #:attributes (terms)
    (pattern ((~literal unquote-splicing) expr:expr)
             #:with terms #'(map unwrap-vterm expr))
    (pattern t:term #:with terms #'(list t.value)))

  ; The syntax classes for patterns and variables are the
  ; same as in basic-node-syntax except that they refer to
  ; n-free-pattern rather than n-node. They ought to be reused.
  (define-syntax-class (term-pattern var-symbols)
    #:description "term pattern"
    #:attributes (value)
    (pattern a:atom #:with value #'a.value)
    (pattern symbol:id
             #:with value
             #`(if (set-member? #,var-symbols (quote symbol))
                   (term n-free-pattern (var-ref (quote symbol)))
                   (term n-free-pattern (term (quote symbol) (args)))))
    (pattern (symbol:id (~var arg-terms (term-pattern var-symbols)) ...)
             #:with value
             #'(term n-free-pattern
                     (pattern (quote symbol)
                              (args ,arg-terms.value ...)))))

  (define-syntax-class variable
    #:description "variable in rule"
    #:attributes (var var-symbol)
    (pattern [var-symbol:id sort-symbol:id]
             #:with var
             #'(term n-free-pattern
                     (var (quote var-symbol) (quote sort-symbol))))
    (pattern [var-symbol:id sort-symbol:id (~datum ...)]
             #:with var
             #'(term n-free-pattern
                     (svar (quote var-symbol) (quote sort-symbol) false)))
    (pattern [var-symbol:id sort-symbol:id (~datum ?...)]
             #:with var
             #'(term n-free-pattern
                     (svar (quote var-symbol) (quote sort-symbol) true))))

  (define-splicing-syntax-class variable-list
    #:description "variable list in a rule"
    #:attributes (value symbols)
    (pattern (~seq #:var v:variable)
             #:with value #'(term n-free-pattern (vars ,v.var))
             #:with symbols #'(set (quote v.var-symbol)))
    (pattern (~seq #:vars (v:variable ...))
             #:with value #'(term n-free-pattern (vars ,v.var ...))
             #:with symbols #'(set (quote v.var-symbol) ...))))

(define-syntax (meta-term stx)
  (syntax-parse stx
    [(_  expr:term)
     #'(meta:make-vterm meta:n-term expr.value)]))

(define-syntax (term stx)
  (syntax-parse stx
    [(_ node:expr expr:term)
     #'(meta:meta-down node (meta-term expr))]))

(define-syntax (pattern stx)
  (syntax-parse stx
    [(_ node:expr
        (~optional vars:variable-list
                   #:defaults ([vars.value #'(term n-free-pattern (vars))]
                               [vars.symbols #'(set)]))
        (~var meta-pattern (term-pattern #'var-symbols)))
     #'(meta:meta-down node
                       (term n-free-pattern
                             (free-pattern
                              ,vars.value
                              ,(let ([var-symbols vars.symbols])
                                 meta-pattern.value))))]))
