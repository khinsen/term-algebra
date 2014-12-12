#lang racket

(require (prefix-in modules: term-algebra/modules)
         (prefix-in builtin: term-algebra/builtin)
         (only-in term-algebra/rewrite reduce))

(reduce (modules:term builtin:truth (== false false)))
(reduce (modules:term builtin:truth (== false true)))
(reduce (modules:term builtin:truth (== true true)))
