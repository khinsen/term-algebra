#lang racket

(require (prefix-in modules: term-algebra/modules)
         (prefix-in builtin: term-algebra/builtin)
         (prefix-in terms: term-algebra/terms)
         (only-in term-algebra/rewrite reduce))

(modules:define-module boolean

  (use builtin:truth)
 
  (op (not x))
  (=-> #:vars [X Y] (not Y) false))

(modules:define-module boolean2
  (extend boolean))
