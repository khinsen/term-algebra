#lang racket

(provide define-module term
         define-meta-module meta-term
         reduce
         (all-from-out term-algebra/builtin))

(require term-algebra/syntax
         (prefix-in modules: term-algebra/modules)
         (prefix-in builtin: term-algebra/builtin)
         (prefix-in rewrite: term-algebra/rewrite))

; Reduce a validated term

(define (reduce vterm)
  (let ([mod (modules:vterm-module vterm)]
        [term (modules:vterm-term vterm)])
    (modules:make-vterm mod (rewrite:reduce term mod))))
