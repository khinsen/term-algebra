#lang racket

(provide term meta-term m-term)

(require (prefix-in modules: term-algebra/modules)
         (prefix-in terms: term-algebra/terms)
         (prefix-in meta: term-algebra/meta)
         (for-syntax syntax/parse))

(define (tterm op args)
  (terms:make-term op args (modules:module-ops meta:m-term)))

(define (unwrap-vterm vterm)
  (cond
    [(meta:vterm? vterm)
     (meta:vterm-term (meta:meta-up vterm))]
    [(or (symbol? vterm)
         (string? vterm)
         (number? vterm))
     vterm]
    [else (error "illegal term " vterm)]))

(define m-term
  (meta:check-module
   (meta:make-vterm meta:m-module
                    (terms:term 'builtin-module (list 'm-term) 'Module))))

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
    (pattern t:term #:with terms #'(list t.value))))

(define-syntax (meta-term stx)
  (syntax-parse stx
    [(_  expr:term)
     #'(meta:make-vterm meta:m-term expr.value)]))

(define-syntax (term stx)
  (syntax-parse stx
    [(_ module:expr expr:term)
     #'(meta:meta-down module (meta-term expr))]))
