#lang racket

(provide meta-term meta-pattern meta-module
         meta-up meta-down)

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators)
         (prefix-in terms: term-algebra/terms)
         (prefix-in modules: term-algebra/modules)
         (prefix-in builtin: term-algebra/builtin)
         (prefix-in rules: term-algebra/rules)
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
  (op (var-ref Symbol) Term)

  (op (args Term ...) ArgList)
  (op (args) ArgList))

(modules:define-builtin-module meta-pattern

  (extend meta-term)

  (sorts Pattern PatternArgList)
  (subsorts [Term Pattern] [ArgList PatternArgList])

  (op (pattern Symbol PatternArgList) Pattern)
  (op (args Pattern ...) PatternArgList)
  (op (head-tail Pattern Pattern) PatternArgList))

(modules:define-builtin-module meta-module

  (extend meta-pattern)

  (sorts Module
         ImportList Import
         SortList SubsortList Subsort
         OpList Op Domain
         RuleList Rule VarList Var)

  (op (module Symbol ImportList SortList SubsortList OpList RuleList) Module)

  (op (imports Import ...) ImportList)
  (op (imports) ImportList)
  (op (use String) Import)
  (op (extend String) Import)

  (op (sorts Symbol ...) SortList)
  (op (sorts) SortList)

  (op (subsorts Subsort ...) SubsortList)
  (op (subsorts) SubsortList)
  (op (subsort Symbol Symbol) Subsort)

  (op (ops Op ...) OpList)
  (op (ops) OpList)
  (op (op Symbol Domain Symbol) Op)
  (op (fixed-arity-domain Symbol ...) Domain)
  (op (fixed-arity-domain) Domain)
  (op (var-arity-domain Symbol) Domain)

  (op (rules Rule ...) RuleList)
  (op (rules) RuleList)
  (op (=-> VarList Pattern Term Term) Rule)
  (op (vars Var ...) VarList)
  (op (vars) VarList)
  (op (var Symbol Symbol) Var)
  (op no-condition Term))

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

  (define (rule-list module rule-terms)

    (define (var-hash var-terms)
      (for/fold ([vars (hash)])
                ([var-term var-terms])
        (match var-term
          [(mterm 'var (list var sort))
           (hash-set vars var sort)]
          [_ (error "Invalid var term " var-term)])))

    (define (pattern-from-meta module vars meta-term)
      (match meta-term
        [(or (mterm 'term (list op (mterm 'args args)))
             (mterm 'pattern (list op (mterm 'args args))))
         (terms:make-term op
                          (map (λ (arg)
                                 (pattern-from-meta module vars arg)) args)
                          (modules:module-ops module))]
        [(mterm 'pattern (list op (mterm 'head-tail (list head tail))))
         (terms:make-ht-pattern op
                                (pattern-from-meta module vars head)
                                (pattern-from-meta module vars tail)
                                (modules:module-ops module))]
        [(mterm 'var-ref (list var-name))
         (terms:var var-name (hash-ref vars var-name))]
        [(mterm 'no-condition empty)
         #f]
        [_ (terms:make-special-term meta-term (modules:module-ops module))]))

    (for/list ([rule rule-terms])
      (match rule
        [(mterm '=-> (list (mterm 'vars vars) pattern condition replacement))
         (let* ([vars (var-hash vars)]
                [rule-pattern (pattern-from-meta module vars pattern)]
                [rule-condition (pattern-from-meta module vars condition)]
                [rule-replacement (pattern-from-meta module vars replacement)])
           (rules:make-rule (modules:module-ops module)
                            vars rule-pattern rule-condition rule-replacement))]
        [_ (error "Invalid rule term " rule)])))

  (match meta-terms
    [(mterm 'module (list name
                          (mterm 'imports import-terms)
                          (mterm 'sorts sort-symbols)
                          (mterm 'subsorts subsort-terms)
                          (mterm 'ops operator-terms)
                          (mterm 'rules rule-terms)))
     (let* ([mod (modules:make-module name meta-terms
                                      (import-list import-terms)
                                      sort-symbols (subsort-list subsort-terms)
                                      (op-list operator-terms)
                                      empty empty)]
            [rules (modules:module-rules mod)]
            [hashcode (modules:module-hashcode mod)])
       (for ([rule (rule-list mod rule-terms)])
         (rules:add-rule! rule hashcode rules))
       mod)]
    [_ (error "Invalid meta module " meta-terms)]))

(define (meta-down module a-term)
  (match a-term
    [(mterm 'term (list op-symbol (mterm 'args args)))
     (term-from-meta module op-symbol args)]
    [(mterm 'module args)
     #:when (eq? module meta-module)
     (module-from-meta a-term)]
    [(terms:term _ _ _)
     (error "Invalid meta-term " a-term)]
    [_ (terms:make-special-term a-term (modules:module-ops module))]))
