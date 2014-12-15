#lang racket

(require (prefix-in modules: term-algebra/modules)
         (prefix-in builtin: term-algebra/builtin)
         (prefix-in terms: term-algebra/terms)
         (only-in term-algebra/rewrite reduce)
         (for-syntax syntax/parse)
         ;; syntax/parse
         racket/match)

(define t (modules:term builtin:equality (== false false)))

(match t
  [(terms:term (terms:op '== _ _) _) 'yes]
  [_ 'no])

(match t
  [(terms:term (== (terms:module-op builtin:equality '==)) _) 'yes]
  [_ 'no])

(match t
  [(struct* terms:term ([op (== (terms:module-op builtin:equality '==) eq?)])) 'yes]
  [_ 'no])

(define-match-expander term
  (lambda (stx)
    (syntax-parse stx
      [(_ an-op some-args)
       #'(struct* terms:term ([op (==  an-op eq?)] [args some-args]))])))

(define-match-expander term0
  (lambda (stx)
    (syntax-parse stx
      [(_ an-op)
       #'(struct* terms:term ([op (==  an-op eq?)] [args (list)]))])))

(match t
  [(term (terms:module-op builtin:equality '==)
         (list (term0 (terms:module-op builtin:equality 'false)) b))
   b]
  [_ 'no])
