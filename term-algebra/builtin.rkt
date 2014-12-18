#lang racket

(provide meta truth equality string symbol exact-number)

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
  (op (== term1 term2) Boolean))

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
  (let ([sorts (foldl sorts:add-sort (sorts:empty-sort-graph)
                      '(ExactNumber))]
        [ops (hash '+ (terms:op '+ '(x y) 'ExactNumber (lambda (x y) (+ x y)))
                   '- (terms:op '- '(x y) 'ExactNumber (lambda (x y) (- x y)))
                   '* (terms:op '* '(x y) 'ExactNumber (lambda (x y) (* x y)))
                   '/ (terms:op '/ '(x y) 'ExactNumber (lambda (x y) (/ x y)))
                   'div (terms:op 'div '(x y) 'ExactNumber
                                  (lambda (x y) (quotient x y)))
                   '> (terms:op '> '(x y) 'Boolean
                                (lambda (x y) (if (> x y)
                                             true
                                             false)))
                   '< (terms:op '< '(x y) 'Boolean
                                (lambda (x y) (if (< x y)
                                             true
                                             false)))
                   '>= (terms:op '>= '(x y) 'Boolean
                                 (lambda (x y) (if (>= x y)
                                              true
                                              false)))
                   '<= (terms:op '<= '(x y) 'Boolean
                                 (lambda (x y) (if (<= x y)
                                              true
                                              false)))
                   '= (terms:op '= '(x y) 'Boolean
                                (lambda (x y) (if (= x y)
                                             true
                                             false)))
                   'true (modules:op-from truth 'true)
                   'false (modules:op-from truth 'false))])
    (modules:make-special-module 'exact-number
                                 (sorts:empty-sort-graph)
                                 ops (set) (set 'exact-number))))
