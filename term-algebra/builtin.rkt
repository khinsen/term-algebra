#lang racket

(provide meta truth equality string symbol exact-number)

(require (prefix-in terms: term-algebra/terms)
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in modules: term-algebra/modules))

(define meta modules:meta)

(modules:define-module truth
  (op true)
  (op false))

(define true (terms:term (modules:op-from truth 'true) '()))
(define false (terms:term (modules:op-from truth 'false) '()))

(modules:define-module equality
  (use truth)
  (op (== term1 term2)))

(modules:make-special-rule equality '==
   (lambda (term1 term2)
     (if (equal? term1 term2)
         true
         false)))

(define string (modules:make-special-module 'string
                                            (sorts:empty-sort-graph)
                                            (hash) (set) (set 'string)))

(define symbol (modules:make-special-module 'symbol
                                            (sorts:empty-sort-graph)
                                            (hash) (set) (set 'symbol)))

(define exact-number
  (let ([ops (hash '+ (terms:op '+ '(x y) (lambda (x y) (+ x y)))
                   '- (terms:op '- '(x y) (lambda (x y) (- x y)))
                   '* (terms:op '* '(x y) (lambda (x y) (* x y)))
                   '/ (terms:op '/ '(x y) (lambda (x y) (/ x y)))
                   'div (terms:op 'div '(x y) (lambda (x y) (quotient x y)))
                   '> (terms:op '> '(x y)
                                (lambda (x y) (if (> x y)
                                             true
                                             false)))
                   '< (terms:op '< '(x y)
                                (lambda (x y) (if (< x y)
                                             true
                                             false)))
                   '>= (terms:op '>= '(x y)
                                 (lambda (x y) (if (>= x y)
                                              true
                                              false)))
                   '<= (terms:op '<= '(x y)
                                 (lambda (x y) (if (<= x y)
                                              true
                                              false)))
                   '= (terms:op '= '(x y)
                                (lambda (x y) (if (= x y)
                                             true
                                             false)))
                   'true (modules:op-from truth 'true)
                   'false (modules:op-from truth 'false)
                   )])
    (modules:make-special-module 'exact-number
                                 (sorts:empty-sort-graph)
                                 ops (set) (set 'exact-number))))
