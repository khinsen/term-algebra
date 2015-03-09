#lang racket

(provide define-module term
         define-meta-module meta-module meta-term
         meta-up meta-down
         m-term m-pattern m-module
         reduce
         (all-from-out term-algebra/builtin))

(require term-algebra/syntax
         (only-in term-algebra/meta meta-up meta-down
                                    m-term m-pattern m-module)
         (prefix-in modules: term-algebra/modules)
         (prefix-in builtin: term-algebra/builtin)
         (prefix-in rewrite: term-algebra/rewrite))

; Reduce a validated term

(define (reduce vterm)
  (let ([mod (modules:vterm-module vterm)]
        [term (modules:vterm-term vterm)])
    (modules:make-vterm mod (rewrite:reduce term mod))))
