#lang racket

(provide meta-term meta-module
         meta-up meta-down)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in terms: term-algebra/terms)
         (prefix-in modules: term-algebra/modules)
         (prefix-in builtin: term-algebra/builtin)
         (for-syntax syntax/parse))

;
; The meta-representation of terms and modules as terms
;
(modules:define-builtin-module meta-term

  (use builtin:rational)
  (use builtin:string)
  (use builtin:symbol)

  (sorts Term ArgList)

  (subsorts [Rational Term] [String Term] [Symbol Term])

  (op (term Symbol ArgList) Term)
  (op (args Term ...) ArgList))

(modules:define-builtin-module meta-module

  (use meta-term)

  (sorts Module
         ImportList Import
         SortList SubsortList Subsort
         OpList Op Domain
         RuleList Rule VarList Var)

  (op (module Symbol ImportList SortList SubsortList OpList RuleList) Module)

  (op (imports Import ...) ImportList)
  (op (use String) Import)
  (op (extend String) Import)

  (op (sorts Symbol ...) SortList)

  (op (subsorts Subsort ...) SubsortList)
  (op (subsort Symbol Symbol) Subsort)

  (op (ops Op ...) OpList)
  (op (op Symbol Domain Symbol) Op)
  (op (fixed-arity-domain Symbol ...) Domain)
  (op (var-arity-domain Symbol) Domain)

  (op (rules Rule ...) RuleList)
  (op (=-> VarList Term Term) Rule)
  (op (=->? VarList Term Term Term) Rule)
  (op (vars Var ...) VarList)
  (op (var Symbol Symbol) Var))

(define meta-term-ops (modules:module-ops meta-term))
(define meta-module-ops (modules:module-ops meta-module))

(define (meta-up a-term)
  (cond
   [(terms:term? a-term)
    (terms:make-term 'term
                     (list (terms:term-op a-term)
                           (terms:make-term 'args
                                            (map meta-up
                                                 (terms:term-args a-term))
                                            meta-term-ops))
                     meta-term-ops)]
   [(modules:module? a-term) (modules:module-meta a-term)]
   [else a-term]))

;
; Parse meta-terms into concrete terms
;
(define-match-expander mterm
  (lambda (stx)
    (syntax-parse stx
      [(_ an-op some-args)
       #'(struct* terms:term ([op (==  an-op equal?)] [args some-args]))])))

(define-match-expander mterm0
  (lambda (stx)
    (syntax-parse stx
      [(_ an-op)
       #'(struct* terms:term ([op (==  an-op equal?)] [args (list)]))])))

(define (term-from-meta module op-symbol args)
  (terms:make-term op-symbol (map (λ (arg) (meta-down module arg)) args)
                   (modules:module-ops module)))

(define (module-from-meta meta-terms)
  
  (define (subsort-list subsort-terms)
    (for/list ([ss subsort-terms])
      (match ss
        [(mterm 'subsort (list s1 s2))
         (cons s1 s2)]
        [_ (error "Invalid subsort term " ss)])))

  (define (op-list op-terms)
    (for/list ([op op-terms])
      (match op
        [(mterm 'op (list name (mterm 'fixed-arity-domain sort-symbols) range))
         (list name sort-symbols range (set))]
        [(mterm 'op (list name (mterm 'var-arity-domain sort-symbols) range))
         (list name sort-symbols range (set 'var-arity))]
        [_ (error "Invalid op term " op)])))

  (define (import-list import-terms)
    (for/list ([import import-terms])
      (match import
        [(mterm 'use (list hashcode))
         (cons (modules:lookup-module-hash hashcode) #t)]
        [(mterm 'extend (list hashcode))
         (cons (modules:lookup-module-hash hashcode) #f)]
        [_ (error "Invalid import term " import)])))
  
  (match meta-terms
    [(mterm 'module (list name
                          (mterm 'imports imports)
                          (mterm 'sorts sort-symbols)
                          (mterm 'subsorts subsorts)
                          (mterm 'ops operators)
                          (mterm 'rules rules)))
     (let* ([sorts (for/fold ([sorts (sorts:empty-sort-graph)])
                             ([import (import-list imports)])
                     (sorts:merge-sort-graph
                      (operators:op-set-sorts (modules:module-ops (car import)))
                      sorts))]
            [sorts (foldl sorts:add-sort sorts sort-symbols)]
            [sorts (for/fold ([sorts sorts])
                             ([sort-pair (subsort-list subsorts)])
                     (sorts:add-subsort (car sort-pair) (cdr sort-pair) sorts))]
            [ops (for/fold ([ops (operators:empty-op-set sorts)]) 
                           ([import (import-list imports)])
                   (operators:merge-op-set (modules:module-ops (car import))
                                           (cdr import)
                                           ops))]
            [ops (for/fold ([ops ops])
                           ([op-spec (op-list operators)])
                   (match-let ([(list symbol domain range properties)
                                op-spec])
                     (operators:add-op symbol domain range
                                       properties ops)))]
            [rules (hash)])
       (modules:make-module name ops rules meta-terms))]
    [_ (error "Invalid meta module " meta-terms)]))

(define (meta-down module a-term)
  (match a-term
    [(mterm 'term (list op-symbol (mterm 'args args)))
     (term-from-meta module op-symbol args)]
    [(mterm 'module args)
     #:when (eq? module meta-module)
     (module-from-meta a-term)]
    [(terms:term _ _ _ _)
     (error "invalid meta-term " a-term)]
    [_ (terms:make-special-term a-term (modules:module-ops module))]))
