#lang racket

(require term-algebra/api
         (prefix-in library: term-algebra/library))

(define-module module-rewrite
  (use m-module)
  (use builtin:equality)
  (use library:boolean)
  
  ;
  ; Cons definitions for all lists - should perhaps be moved to m-module
  ;
  (op (cons Import ImportList) ImportList)
  (=-> #:vars ([I Import] [Is Import ...])
       (cons I (imports Is))
       (imports I Is))
  (op (cons Symbol SortList) SortList)
  (=-> #:vars ([S Symbol] [Ss Symbol ...])
       (cons S (sorts Ss))
       (sorts S Ss))
  (op (cons Subsort SubsortList) SubsortList)
  (=-> #:vars ([S Subsort] [Ss Subsort ...])
       (cons S (subsorts Ss))
       (subsorts S Ss))
  (op (cons Op OpList) OpList)
  (=-> #:vars ([O Op] [Os Op ...])
       (cons O (ops Os))
       (ops O Os))
  (op (cons Rule RuleList) RuleList)
  (=-> #:vars ([R Rule] [Rs Rule ...])
       (cons R (rules Rs))
       (rules R Rs))
  (op (cons Var VarList) VarList)
  (=-> #:vars ([V Var] [Vs Var ...])
       (cons V (vars Vs))
       (vars V Vs))
  ; A bit different - only one op is list-like. Perhaps there should
  ; be a subsort for fixed-arity domains.
  (op (cons Symbol Domain) Domain)
  (=-> #:vars ([S Symbol] [Ss Symbol ...])
       (cons S (fixed-arity-domain Ss))
       (fixed-arity-domain S Ss))

  ;
  ; rename-sort
  ;
  (op (rename-sort Module Symbol Symbol) Module)
  (=-> #:vars ([Name Symbol] [Imports ImportList]
               [Sorts SortList] [Subsorts SubsortList]
               [Ops OpList] [Rules RuleList]
               [S1 Symbol] [S2 Symbol])
       (rename-sort (module Name Imports Sorts Subsorts Ops Rules) S1 S2)
       (module Name Imports
         (rename-sort Sorts S1 S2)
         (rename-sort Subsorts S1 S2)
         (rename-sort Ops S1 S2)
         (rename-sort Rules S1 S2)))

  ;
  ; rename-sort: map-like application to lists
  ;
  (op (rename-sort SortList Symbol Symbol) SortList)
  (=-> #:vars ([S1 Symbol] [S2 Symbol])
       (rename-sort (sorts) S1 S2)
       (sorts))
  (=-> #:vars ([S Symbol] [S1 Symbol] [S2 Symbol] [Ss Symbol ...])
       (rename-sort (sorts S Ss) S1 S2)
       (cons (rename-sort S S1 S2) (rename-sort (sorts Ss) S1 S2)))

  (op (rename-sort SubsortList Symbol Symbol) SubsortList)
  (=-> #:vars ([S1 Symbol] [S2 Symbol])
       (rename-sort (subsorts) S1 S2)
       (subsorts))
  (=-> #:vars ([S Subsort] [Ss Subsort ...] [S1 Symbol] [S2 Symbol])
       (rename-sort (subsorts S Ss) S1 S2)
       (cons (rename-sort S S1 S2) (rename-sort (subsorts Ss) S1 S2)))

  (op (rename-sort OpList Symbol Symbol) OpList)
  (=-> #:vars ([S1 Symbol] [S2 Symbol])
       (rename-sort (ops) S1 S2)
       (ops))
  (=-> #:vars ([O Op] [Os Op ...] [S1 Symbol] [S2 Symbol])
       (rename-sort (ops O Os) S1 S2)
       (cons (rename-sort O S1 S2) (rename-sort (ops Os) S1 S2)))

  (op (rename-sort RuleList Symbol Symbol) RuleList)
  (=-> #:vars ([S1 Symbol] [S2 Symbol])
       (rename-sort (rules) S1 S2)
       (rules))
  (=-> #:vars ([R Rule] [Rs Rule ...] [S1 Symbol] [S2 Symbol])
       (rename-sort (rules R Rs) S1 S2)
       (cons (rename-sort R S1 S2) (rename-sort (rules Rs) S1 S2)))

  (op (rename-sort VarList Symbol Symbol) VarList)
  (=-> #:vars ([S1 Symbol] [S2 Symbol])
       (rename-sort (vars) S1 S2)
       (vars))
  (=-> #:vars ([V Var] [Vs Var ...] [S1 Symbol] [S2 Symbol])
       (rename-sort (vars V Vs) S1 S2)
       (cons (rename-sort V S1 S2) (rename-sort (vars Vs) S1 S2)))

  ;
  ; rename-sort: the operations on the leaves of the tree
  ;
  (op (rename-sort Subsort Symbol Symbol) Subsort)
  (=-> #:vars ([SA Symbol] [SB Symbol] [S1 Symbol] [S2 Symbol])
       (rename-sort (subsort SA SB) S1 S2)
       (subsort (rename-sort SA S1 S2) (rename-sort SB S1 S2)))
  
  (op (rename-sort Op Symbol Symbol) Op)
  (=-> #:vars ([N Symbol] [D Domain] [S Symbol] [S1 Symbol] [S2 Symbol])
       (rename-sort (op N D S) S1 S2)
       (op N (rename-sort D S1 S2) (rename-sort S S1 S2)))

  (op (rename-sort Domain Symbol Symbol) Domain)
  (=-> #:vars ([S1 Symbol] [S2 Symbol])
       (rename-sort (fixed-arity-domain) S1 S2)
       (fixed-arity-domain))
  (=-> #:vars ([S Symbol] [Ss Symbol ...] [S1 Symbol] [S2 Symbol])
       (rename-sort (fixed-arity-domain S Ss) S1 S2)
       (cons (rename-sort S S1 S2) (rename-sort (fixed-arity-domain Ss) S1 S2)))
  (=-> #:vars ([S Symbol] [S1 Symbol] [S2 Symbol])
       (rename-sort (var-arity-domain S) S1 S2)
       (var-arity-domain (rename-sort S S1 S2)))

  (op (rename-sort Rule Symbol Symbol) Rule)
  (=-> #:vars ([Vs VarList] [P1 Pattern] [P2 Pattern] [P3 Pattern]
               [S1 Symbol] [S2 Symbol])
       (rename-sort (=-> Vs P1 P2 P3) S1 S2)
       (=-> (rename-sort Vs S1 S2) P1 P2 P3))
  
  (op (rename-sort Var Symbol Symbol) Var)
  (=-> #:vars ([N Symbol] [S1 Symbol] [S2 Symbol])
       (rename-sort (var N S1) S1 S2)
       (var N S2))
  (=-> #:vars ([N Symbol] [S1 Symbol] [S2 Symbol])
       (rename-sort (svar N S1) S1 S2)
       (svar N S2))
  (=-> #:vars ([V Var] [S1 Symbol] [S2 Symbol])
       (rename-sort V S1 S2)
       V)

  (op (rename-sort Symbol Symbol Symbol) Symbol)
  (=-> #:vars ([S1 Symbol] [S2 Symbol]) (rename-sort S1 S1 S2) S2)
  (=-> #:vars ([S Symbol] [S1 Symbol] [S2 Symbol]) (rename-sort S S1 S2) S)
  
  ;; ; remove-duplicates
  ;; (op (remove-duplicates SortList) SortList)
  ;; (=-> (remove-duplicates (sorts))
  ;;      (sorts))
  ;; (=-> #:vars ([S Symbol] [Ss Symbol ...])
  ;;      (remove-duplicates (sorts S Ss))
  ;;      #:if (contains? S (sorts Ss))
  ;;      (remove-duplicates (sorts Ss)))
  ;; (=-> #:vars ([S Symbol] [Ss Symbol ...])
  ;;      (remove-duplicates (sorts S Ss))
  ;;      (cons S (remove-duplicates (sorts Ss))))

  ;; (op (contains? Symbol SortList) Boolean)
  ;; (=-> #:vars ([S Symbol])
  ;;      (contains? S (sorts))
  ;;      false)
  ;; (=-> #:vars ([S Symbol] [Ss Symbol ...])
  ;;      (contains? S (sorts S Ss))
  ;;      true)
  ;; (=-> #:vars ([S Symbol] [S1 Symbol] [Ss Symbol ...])
  ;;      (contains? S (sorts S1 Ss))
  ;;      (contains? S (sorts Ss)))

  ;; (op (remove-duplicates SubsortList) SubsortList)
  ;; (=-> (remove-duplicates (subsorts))
  ;;      (subsorts))
  ;; (=-> #:vars ([S1 Symbol] [S2 Symbol] [Ss Subsort ...])
  ;;      (remove-duplicates (subsorts (subsort S1 S2) Ss))
  ;;      #:if (or (contains? (subsort S1 S2) (subsorts Ss))
  ;;               (== S1 S2))
  ;;      (remove-duplicates (subsorts Ss)))
  ;; (=-> #:vars ([S Subsort] [Ss Subsort ...])
  ;;      (remove-duplicates (subsorts S Ss))
  ;;      (cons S (remove-duplicates (subsorts Ss))))

  ;; (op (contains? Subsort SubsortList) Boolean)
  ;; (=-> #:vars ([S Subsort])
  ;;      (contains? S (subsorts))
  ;;      false)
  ;; (=-> #:vars ([S Subsort] [Ss Subsort ...])
  ;;      (contains? S (subsorts S Ss))
  ;;      true)
  ;; (=-> #:vars ([S Subsort] [S1 Subsort] [Ss Subsort ...])
  ;;      (contains? S (subsorts S1 Ss))
  ;;      (contains? S (subsorts Ss)))

  ;
  ; add-import
  ;
  (op (add-import Module Import) Module)
  (=-> #:vars ([Name Symbol] [Imports ImportList]
               [Sorts SortList] [Subsorts SubsortList]
               [Ops OpList] [Rules RuleList]
               [I Import])
       (add-import (module Name Imports  Sorts Subsorts Ops Rules) I)
       (module Name (cons I Imports) Sorts Subsorts Ops Rules))

  ;
  ; module-name
  ;
  (op (module-name Module Symbol) Module)
  (=-> #:vars ([Name Symbol] [Imports ImportList]
               [Sorts SortList] [Subsorts SubsortList]
               [Ops OpList] [Rules RuleList]
               [NN Symbol])
       (module-name (module Name Imports  Sorts Subsorts Ops Rules) NN)
       (module NN Imports Sorts Subsorts Ops Rules))
)

(define-module module-rewrite+

  (include module-rewrite)

  (sorts Transform Transforms)

  ;
  ; Transforms and their application
  ;
  (op (transforms Transform ...) Transforms)
  (op ($apply-transform Transform Module) Module)

  (op (module-name Symbol) Transform)
  (=-> #:vars ([N Symbol] [M Module])
       ($apply-transform (module-name N) M)
       (module-name M N))

  (op (add-import Import) Transform)
  (=-> #:vars ([I Import] [M Module])
       ($apply-transform (add-import I) M)
       (add-import M I))

  (op (rename-sort Symbol Symbol) Transform)
  (=-> #:vars ([S1 Symbol] [S2 Symbol] [M Module])
       ($apply-transform (rename-sort S1 S2) M)
       (rename-sort M S1 S2))

  ;
  ; Extended module
  ;
  (op (module Module Transforms) Module)

  (=-> #:vars ([M Module] [T Transform])
       (module M (transforms T))
       ($apply-transform T M))
  (=-> #:vars ([M Module] [T Transform] [Ts Transform ...])
       (module M (transforms T Ts))
       (module ($apply-transform T M) (transforms Ts)))
  
  ;
  ; Extended use and include
  ;
  (op (use Module Transforms) Import)
  (op (include Module Transforms) Import)

  (=-> #:vars ([M Module] [T Transform])
       (use M (transforms T))
       (use ($apply-transform T M)))
  (=-> #:vars ([M Module] [T Transform] [Ts Transform ...])
       (use M (transforms T Ts))
       (use ($apply-transform T M) (transforms Ts)))
  (=-> #:vars ([M Module] [T Transform])
       (include M (transforms T))
       (include ($apply-transform T M)))
  (=-> #:vars ([M Module] [T Transform] [Ts Transform ...])
       (include M (transforms T Ts))
       (include ($apply-transform T M) (transforms Ts)))

  
)

;; (define-module test
;;   (sort A)
;;   (op (foo A) A)
;;   (op bar A))

;; (define rw (term module-rewrite (rename-sort [[test]] 'A 'B)))
;; (reduce rw)


(define-module list
  
  (sorts Element List NonEmptyList)
  (subsorts [NonEmptyList List])
  
  (op (list Element ...) NonEmptyList)
  (op (list) List)

  (op (head NonEmptyList) Element)
  (=-> #:vars ([E Element] [ES Element ...])
       (head (list E ES))
       E)
  (op (tail NonEmptyList) List)
  (=-> #:vars ([E Element] [ES Element ...])
       (tail (list E ES))
       (list ES))
  
  (op (cons Element List) NonEmptyList)
  (=-> #:vars ([E Element] [ES Element ...])
       (cons E (list ES))
       (list E ES)))

(define sort-list
  (reduce
   (term module-rewrite+
         (module 'list-of-sorts
             (imports (use (builtin-module 'symbol))
                      (use [[list]]
                           (transforms
                            (rename-sort 'Element 'Symbol)
                            (rename-sort 'List 'SortList)
                            (rename-sort 'NonEmptyList 'SortList))))
           (sorts) (subsorts) (ops) (rules)))))

(reduce (term sort-list (cons 'X (list 'A 'B))))

(define sort-list-2
  (reduce
   (term module-rewrite+
         (module [[list]]
             (transforms
              (module-name 'list-of-sorts)
              (add-import (use (builtin-module 'symbol)))
              (rename-sort 'Element 'Symbol)
              (rename-sort 'List 'SortList)
              (rename-sort 'NonEmptyList 'SortList))))))

(reduce (term sort-list-2 (cons 'X (list 'A 'B))))
