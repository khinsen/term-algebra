#lang racket

(provide node-transforms)

(require term-algebra/basic-api)

(define-node node-transforms

  (use builtin:node)
  (use builtin:equality)
  
  (sorts Transform Transforms)
  
  ;
  ; Cons definitions for all lists
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
  (op (cons Term ArgList) ArgList)
  (op (cons Pattern PatternArgList) PatternArgList)
  (=-> #:vars ([A Pattern] [As Pattern ...])
       (cons A (args As))
       (args A As))
  ; A bit different - only one op is list-like. Perhaps there should
  ; be a subsort for fixed-arity domains.
  (op (cons Symbol Domain) Domain)
  (=-> #:vars ([S Symbol] [Ss Symbol ...])
       (cons S (domain Ss))
       (domain S Ss))

  ;
  ; rename-sort
  ;
  (op (rename-sort Node Symbol Symbol) Node)
  (=-> #:vars ([Name Symbol] [Imports ImportList]
               [Sorts SortList] [Subsorts SubsortList]
               [Ops OpList] [Rules RuleList]
               [S1 Symbol] [S2 Symbol])
       (rename-sort (node Name Imports Sorts Subsorts Ops Rules) S1 S2)
       (node Name Imports
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
  (=-> #:vars ([N Symbol] [D VarLengthDomain] [S Symbol] [S1 Symbol] [S2 Symbol])
       (rename-sort (symop N D S) S1 S2)
       (symop N (rename-sort D S1 S2) (rename-sort S S1 S2)))

  (op (rename-sort Domain Symbol Symbol) Domain)
  (op (rename-sort VarLengthDomain Symbol Symbol) VarLengthDomain)
  (=-> #:vars ([S1 Symbol] [S2 Symbol])
       (rename-sort (domain) S1 S2)
       (domain))
  (=-> #:vars ([S Symbol] [Ss Symbol ...] [S1 Symbol] [S2 Symbol])
       (rename-sort (domain S Ss) S1 S2)
       (cons (rename-sort S S1 S2) (rename-sort (domain Ss) S1 S2)))
  (=-> #:vars ([S Symbol] [S1 Symbol] [S2 Symbol])
       (rename-sort (vl-domain S) S1 S2)
       (vl-domain (rename-sort S S1 S2)))

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
  
  ;
  ; rename-op
  ;
  (op (rename-op Node Symbol Symbol) Node)
  (=-> #:vars ([Name Symbol] [Imports ImportList]
               [Sorts SortList] [Subsorts SubsortList]
               [Ops OpList] [Rules RuleList]
               [OS1 Symbol] [OS2 Symbol])
       (rename-op (node Name Imports Sorts Subsorts Ops Rules) OS1 OS2)
       (node Name Imports Sorts Subsorts
         (rename-op Ops OS1 OS2)
         (rename-op Rules OS1 OS2)))

  ;
  ; rename-op: map-like application to lists
  ;
  (op (rename-op OpList Symbol Symbol) OpList)
  (=-> #:vars ([OS1 Symbol] [OS2 Symbol])
       (rename-op (ops) OS1 OS2)
       (ops))
  (=-> #:vars ([O Op] [Os Op ...] [OS1 Symbol] [OS2 Symbol])
       (rename-op (ops O Os) OS1 OS2)
       (cons (rename-op O OS1 OS2) (rename-op (ops Os) OS1 OS2)))

  (op (rename-op RuleList Symbol Symbol) RuleList)
  (=-> #:vars ([OS1 Symbol] [OS2 Symbol])
       (rename-op (rules) OS1 OS2)
       (rules))
  (=-> #:vars ([R Rule] [Rs Rule ...] [OS1 Symbol] [OS2 Symbol])
       (rename-op (rules R Rs) OS1 OS2)
       (cons (rename-op R OS1 OS2) (rename-op (rules Rs) OS1 OS2)))

  (op (rename-op PatternArgList Symbol Symbol) PatternArgList)
  (op (rename-op ArgList Symbol Symbol) ArgList)
  (=-> #:vars ([OS1 Symbol] [OS2 Symbol])
       (rename-op (args) OS1 OS2)
       (args))
  (=-> #:vars ([A Pattern] [As Pattern ...] [OS1 Symbol] [OS2 Symbol])
       (rename-op (args A As) OS1 OS2)
       (cons (rename-op A OS1 OS2) (rename-op (args As) OS1 OS2)))

  ;
  ; rename-op: the operations on the leaves of the tree
  ;
  (op (rename-op Op Symbol Symbol) Op)
  (=-> #:vars ([D Domain] [S Symbol] [OS1 Symbol] [OS2 Symbol])
       (rename-op (op OS1 D S) OS1 OS2)
       (op OS2 D S))
  (=-> #:vars ([N Symbol] [D Domain] [S Symbol] [OS1 Symbol] [OS2 Symbol])
       (rename-op (op N D S) OS1 OS2)
       (op N D S))
  (=-> #:vars ([D VarLengthDomain] [S Symbol] [OS1 Symbol] [OS2 Symbol])
       (rename-op (symop OS1 D S) OS1 OS2)
       (symop OS2 D S))
  (=-> #:vars ([N Symbol] [D VarLengthDomain] [S Symbol] [OS1 Symbol] [OS2 Symbol])
       (rename-op (symop N D S) OS1 OS2)
       (symop N D S))

  (op (rename-op Rule Symbol Symbol) Rule)
  (=-> #:vars ([Vs VarList] [P1 Pattern] [P2 Pattern] [P3 Pattern]
               [OS1 Symbol] [OS2 Symbol])
       (rename-op (=-> Vs P1 P2 P3) OS1 OS2)
       (=-> Vs (rename-op P1 OS1 OS2)
               (rename-op P2 OS1 OS2)
               (rename-op P3 OS1 OS2)))

  (op (rename-op Pattern Symbol Symbol) Pattern)
  (op (rename-op Term Symbol Symbol) Term)
  (=-> #:vars ([A PatternArgList] [OS1 Symbol] [OS2 Symbol])
       (rename-op (pattern OS1 A) OS1 OS2)
       (pattern OS2 (rename-op A OS1 OS2)))
  (=-> #:vars ([OS Symbol] [A PatternArgList] [OS1 Symbol] [OS2 Symbol])
       (rename-op (pattern OS A) OS1 OS2)
       (pattern OS (rename-op A OS1 OS2)))
  (=-> #:vars ([A ArgList] [OS1 Symbol] [OS2 Symbol])
       (rename-op (term OS1 A) OS1 OS2)
       (term OS2 (rename-op A OS1 OS2)))
  (=-> #:vars ([OS Symbol] [A ArgList] [OS1 Symbol] [OS2 Symbol])
       (rename-op (term OS A) OS1 OS2)
       (term OS (rename-op A OS1 OS2)))
  (=-> #:vars ([P Pattern] [OS1 Symbol] [OS2 Symbol])
       (rename-op P OS1 OS2)
       P)

  ;
  ; add-import
  ;
  (op (add-import Node Import) Node)
  (=-> #:vars ([Name Symbol] [Imports ImportList]
               [Sorts SortList] [Subsorts SubsortList]
               [Ops OpList] [Rules RuleList]
               [I Import])
       (add-import (node Name Imports  Sorts Subsorts Ops Rules) I)
       (node Name (cons I Imports) Sorts Subsorts Ops Rules))

  ;
  ; node-name
  ;
  (op (node-name Node Symbol) Node)
  (=-> #:vars ([Name Symbol] [Imports ImportList]
               [Sorts SortList] [Subsorts SubsortList]
               [Ops OpList] [Rules RuleList]
               [NN Symbol])
       (node-name (node Name Imports  Sorts Subsorts Ops Rules) NN)
       (node NN Imports Sorts Subsorts Ops Rules))

  ;
  ; Transforms and their application
  ;
  (op (transforms Transform ...) Transforms)
  (op ($apply-transform Transform Node) Node)

  (op (node-name Symbol) Transform)
  (=-> #:vars ([N Symbol] [M Node])
       ($apply-transform (node-name N) M)
       (node-name M N))

  (op (add-import Import) Transform)
  (=-> #:vars ([I Import] [M Node])
       ($apply-transform (add-import I) M)
       (add-import M I))

  (op (rename-sort Symbol Symbol) Transform)
  (=-> #:vars ([S1 Symbol] [S2 Symbol] [M Node])
       ($apply-transform (rename-sort S1 S2) M)
       (rename-sort M S1 S2))

  (op (rename-op Symbol Symbol) Transform)
  (=-> #:vars ([O1 Symbol] [O2 Symbol] [M Node])
       ($apply-transform (rename-op O1 O2) M)
       (rename-op M O1 O2))

  ;
  ; Extended node
  ;
  (op (transformed-node Node Transforms) Node)

  (=-> #:vars ([M Node] [T Transform])
       (transformed-node M (transforms T))
       ($apply-transform T M))
  (=-> #:vars ([M Node] [T Transform] [Ts Transform ...])
       (transformed-node M (transforms T Ts))
       (transformed-node ($apply-transform T M) (transforms Ts)))
  
  ;
  ; Extended use and include
  ;
  (op (use Node Transforms) Import)
  (op (include Node Transforms) Import)

  (=-> #:vars ([M Node] [T Transform])
       (use M (transforms T))
       (use ($apply-transform T M)))
  (=-> #:vars ([M Node] [T Transform] [Ts Transform ...])
       (use M (transforms T Ts))
       (use ($apply-transform T M) (transforms Ts)))
  (=-> #:vars ([M Node] [T Transform])
       (include M (transforms T))
       (include ($apply-transform T M)))
  (=-> #:vars ([M Node] [T Transform] [Ts Transform ...])
       (include M (transforms T Ts))
       (include ($apply-transform T M) (transforms Ts))))
