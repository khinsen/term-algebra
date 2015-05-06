#lang racket

(provide (struct-out rule) (struct-out equation)
         make-rule make-equation
         empty-rules merge-rules! add-rule! add-proc!)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in terms: term-algebra/terms))

(struct rule (pattern condition replacement)
        #:transparent)

(struct equation (left right condition)
        #:transparent)

;
; Make a rule structure after checking its components
;
(define (make-rule ops vars pattern condition replacement)
  (let* ([vars-in-pattern (terms:vars-in-term pattern)]
         [vars-in-replacement (terms:vars-in-term replacement)]
         [declared-vars
          (list->set
           (hash-map vars (λ (name spec)
                            (case (cdr spec)
                              ['one (terms:var name (car spec))]
                              ['zero-or-more (terms:svar name (car spec) #t)]
                              ['one-or-more  (terms:svar name (car spec) #f)]))))])
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
      (error (format "Term ~s must be of sort ~s"
                     replacement (terms:sort-of pattern))))
    (rule pattern condition replacement)))

;
; Make an equation structure after checking its components
;
(define (make-equation ops vars left right condition)
  (let* ([vars-in-left (terms:vars-in-term left)]
         [vars-in-right (terms:vars-in-term right)]
         [declared-vars
          (list->set
           (hash-map vars (λ (name spec)
                            (case (cdr spec)
                              ['one (terms:var name (car spec))]
                              ['zero-or-more (terms:svar name (car spec) #t)]
                              ['one-or-more  (terms:svar name (car spec) #f)]))))])
    (when condition
      (unless (sorts:is-sort? (terms:sort-of condition) 'Boolean
                              (operators:op-set-sorts ops))
        (error (format "Condition ~s not of sort Boolean" condition)))
      (unless (set-empty?
               (set-subtract (terms:vars-in-term condition) vars-in-left vars-in-right))
        (error (format "Condition ~s contains variables that are not in the left or right patterns" condition))))
    (let ([unused-vars (set-subtract declared-vars vars-in-left vars-in-right)])
      (unless (set-empty? unused-vars)
        (error (format "Var list contains variables ~a that are not used in the equation" (set->list unused-vars)))))
    ; check for equal kinds instead!
    (unless (equal? (sorts:kind (terms:sort-of right)
                                (operators:op-set-sorts ops))
                    (sorts:kind (terms:sort-of left)
                                (operators:op-set-sorts ops)))
      (error (format "Term ~s and ~s must be of the same kind" left right)))
    (equation left right condition)))

;
; Manage complete rule lists as used in nodes
;
(define (empty-rules)
  (make-hash))

(define (merge-rules! to-merge prior-imports rules)
  (hash-for-each to-merge
                 (λ (key value)
                   (define new-rules
                     (for/list ([rule value]
                                #:when (not (hash-has-key? prior-imports
                                                           (cdr rule))))
                       rule))
                   (hash-update! rules
                                 key
                                 (λ (l) (append l new-rules))
                                 empty))))

(define (add-rule! rule origin rules)
  (let* ([pattern (rule-pattern rule)]
         [key (if (terms:term? pattern)
                  (terms:term-op pattern)
                  (terms:sort-of pattern))])
    (hash-update! rules
                  key
                  (λ (l) (append l (list (cons rule origin))))
                  empty)))

(define (add-proc! op-symbol proc origin rules)
  (hash-update! rules
                op-symbol
                ; A procedure replaces all prior (imported) rules
                ; and procedures.
                (λ (l) (list (cons proc origin)))
                empty))
