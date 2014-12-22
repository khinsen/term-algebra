#lang racket

(require (prefix-in modules: term-algebra/modules)
         (prefix-in builtin: term-algebra/builtin)
         (prefix-in terms: term-algebra/terms)
         (only-in term-algebra/rewrite reduce)
         graph)

(modules:define-module test1
  (sort toto)
  (sorts foo bar)
  (subsort foo bar)
  (op zero toto))

(modules:define-module test2
  (use test1)
  (sorts baz quux)
  (subsort baz quux)
  (subsort baz bar)
  (op one baz))

;; (with-output-to-file "/Users/hinsen/Desktop/sort-graph.dot"
;;   #:exists 'replace
;;   (lambda () (display (graphviz (modules:module-sorts test2)))))
