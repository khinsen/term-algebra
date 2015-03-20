#lang racket

(provide define-module module
         define-unchecked-module unchecked-module
         term meta-term
         meta-up meta-down
         m-term m-module
         reduce
         ; builtin modules:
         builtin:any
         builtin:truth builtin:equality
         builtin:string builtin:symbol
         builtin:natural builtin:integer builtin:rational)

(require term-algebra/term-syntax
         term-algebra/module-syntax
         (only-in term-algebra/meta meta-up meta-down)
         (prefix-in terms: term-algebra/terms)
         (prefix-in modules: term-algebra/modules)
         (prefix-in meta: term-algebra/meta)
         (prefix-in rewrite: term-algebra/rewrite))

; Reduce a validated term

(define (reduce vterm)
  (let ([mod (meta:vterm-module vterm)]
        [term (meta:vterm-term vterm)])
    (meta:make-vterm mod (rewrite:reduce term mod))))

; Builtin modules

(define builtin:any
  (meta:check-module
   (term m-module (builtin-module 'any))))

(define builtin:truth
  (meta:check-module
   (term m-module (builtin-module 'truth))))

(define builtin:equality
  (meta:check-module
   (term m-module (builtin-module 'equality))))

(define builtin:string
  (meta:check-module
   (term m-module (builtin-module 'string))))

(define builtin:symbol
  (meta:check-module
   (term m-module (builtin-module 'symbol))))

(define builtin:natural
  (meta:check-module
   (term m-module (builtin-module 'natural))))

(define builtin:integer
  (meta:check-module
   (term m-module (builtin-module 'integer))))

(define builtin:rational
  (meta:check-module
   (term m-module (builtin-module 'rational))))
