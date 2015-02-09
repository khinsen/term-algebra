#lang racket

(provide (struct-out rule)
         make-rule)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in terms: term-algebra/terms))

(struct rule (pattern condition replacement)
        #:transparent)

(define (make-rule ops vars pattern condition replacement)
  (let* ([vars-in-pattern (terms:vars-in-term pattern)]
         [vars-in-replacement (terms:vars-in-term replacement)]
         [declared-vars (list->set
                         (hash-map vars (λ (name sort) (terms:var name sort))))])
    (when condition
      (unless (sorts:is-sort? (terms:sort-of condition) 'Boolean
                              (operators:op-set-sorts ops))
        (error (format "Condition ~s not of sort Boolean" condition)))
      (unless (set-empty?
               (set-subtract (terms:vars-in-term condition) vars-in-pattern))
        (error (format "Condition ~s contains variables that are not in the rule pattern" condition))))
    (unless (set-empty?
             (set-subtract vars-in-replacement vars-in-pattern))
      (error (format "Term ~s contains variables that are not in the rule pattern" replacement)))
    (let ([unused-vars (set-subtract declared-vars vars-in-pattern)])
      (unless (set-empty? unused-vars)
        (error (format "Var list contains variables ~a that are not used in the rule" (set->list unused-vars)))))
    (unless (sorts:is-sort? (terms:sort-of replacement) (terms:sort-of pattern)
                            (operators:op-set-sorts ops))
      (error (format "Term ~s must of sort ~s"
                     replacement (terms:sort-of pattern))))
    (rule pattern condition replacement)))
