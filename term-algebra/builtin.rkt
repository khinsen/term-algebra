#lang racket

(provide meta truth equality string symbol rational)

(require (prefix-in terms: term-algebra/terms)
         (prefix-in sorts: term-algebra/sorts)
         (prefix-in modules: term-algebra/modules))

(define meta modules:meta)

(modules:define-module truth
  (sort Boolean)
  (op true Boolean)
  (op false Boolean))

(define true (terms:term (modules:op-from truth 'true) '()))
(define false (terms:term (modules:op-from truth 'false) '()))

(modules:define-module equality
  (use truth)
  (op (== Any Any) Boolean))

(modules:make-special-rule equality '==
   (lambda (term1 term2)
     (if (equal? term1 term2)
         true
         false)))

(define string
  (let ([sorts (foldl sorts:add-sort (sorts:empty-sort-graph)
                      (list (terms:sort'String)))])
    (modules:make-special-module 'string sorts (hash) (set) (set 'string))))

(define symbol
  (let ([sorts (foldl sorts:add-sort (sorts:empty-sort-graph)
                      (list (terms:sort'Symbol)))])
    (modules:make-special-module 'symbol sorts (hash) (set) (set 'symbol))))

(define rational
  (let* ([rat-sort (terms:sort'Rational)]
         [bool-sort (modules:sort-from truth 'Boolean)]
         [sorts (foldl sorts:add-sort (sorts:empty-sort-graph)
                       (list bool-sort rat-sort))]
         [ops (hash '+ (terms:op '+ (list rat-sort rat-sort) rat-sort
                                 (lambda (x y) (+ x y)))
                    '- (terms:op '- (list rat-sort rat-sort) rat-sort
                                 (lambda (x y) (- x y)))
                    '* (terms:op '* (list rat-sort rat-sort) rat-sort
                                 (lambda (x y) (* x y)))
                    '/ (terms:op '/ (list rat-sort rat-sort) rat-sort
                                 (lambda (x y) (/ x y)))
                    'div (terms:op 'div (list rat-sort rat-sort) rat-sort
                                   (lambda (x y) (quotient x y)))
                    '> (terms:op '> (list rat-sort rat-sort) bool-sort
                                 (lambda (x y) (if (> x y)
                                              true
                                              false)))
                    '< (terms:op '< (list rat-sort rat-sort) bool-sort
                                 (lambda (x y) (if (< x y)
                                              true
                                              false)))
                    '>= (terms:op '>= (list rat-sort rat-sort) bool-sort
                                  (lambda (x y) (if (>= x y)
                                               true
                                               false)))
                    '<= (terms:op '<= (list rat-sort rat-sort) bool-sort
                                  (lambda (x y) (if (<= x y)
                                               true
                                               false)))
                    '= (terms:op '= (list rat-sort rat-sort) bool-sort
                                 (lambda (x y) (if (= x y)
                                              true
                                              false)))
                    'true (modules:op-from truth 'true)
                    'false (modules:op-from truth 'false))])
    (modules:make-special-module 'rational
                                 sorts ops (set) (set 'rational-number))))
