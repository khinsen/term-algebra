#lang racket

(require (prefix-in modules: term-algebra/modules)
         (prefix-in builtin: term-algebra/builtin)
         (prefix-in terms: term-algebra/terms)
         (only-in term-algebra/rewrite reduce)
         graph)

(modules:define-module test
  (sort Foo)
  (op bar Foo)
  (op (baz Foo ...) Foo))

(modules:term test bar)
(modules:term test (baz))
(modules:term test (baz bar))
(modules:term test (baz bar bar))
(modules:term test (baz bar (baz bar) bar))

;; (with-output-to-file "/Users/hinsen/Desktop/sort-graph.dot"
;;   #:exists 'replace
;;   (lambda () (display (graphviz (modules:module-sorts test2)))))
