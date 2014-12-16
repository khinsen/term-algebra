#lang racket

(require (prefix-in modules: term-algebra/modules)
         (prefix-in builtin: term-algebra/builtin)
         (prefix-in terms: term-algebra/terms)
         (only-in term-algebra/rewrite reduce))

(modules:define-module test

  (use builtin:exact-number)
 
  (op zero))

(reduce (modules:term test (+ zero 2)))

