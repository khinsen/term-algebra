#lang racket

(provide node-transforms)

(require term-algebra/basic-api)

(define-node node-transforms

  (include builtin:node)
  (use builtin:equality)

  (sorts Transform Transforms
         Declaration Declarations)
  (subsorts [Import Declaration] [Sorts Declaration] [Subsort Declaration]
            [Subsorts Declaration] [Op Declaration] [Equation Declaration]
            [Rule Declaration])

  ;
  ; Set reductions for all sets
  ;
  (=> #:vars ([I Import] [Is Import ?...])
      (imports I I Is)
      (imports I Is))
  (=> #:vars ([S Symbol] [Ss Symbol ?...])
      (sorts S S Ss)
      (sorts S Ss))
  (=> #:vars ([S Subsort] [Ss Subsort ?...])
      (subsorts S S Ss)
      (subsorts S Ss))
  (=> #:vars ([O Op] [Os Op ?...])
      (ops O O Os)
      (ops O Os))
  (=> #:vars ([E Equation] [Es Equation ?...])
      (equations E E Es)
      (equations E Es))
  (=> #:vars ([V Var] [Vs Var ?...])
      (vars V V Vs)
      (vars V Vs))

  ;
  ; Cons definitions for all lists and sets
  ;
  (op (cons Import Imports) Imports)
  (=> #:vars ([I Import] [Is Import ?...])
      (cons I (imports Is))
      (imports I Is))
  (op (cons Symbol Sorts) Sorts)
  (=> #:vars ([S Symbol] [Ss Symbol ?...])
      (cons S (sorts Ss))
      (sorts S Ss))
  (op (cons Subsort Subsorts) Subsorts)
  (=> #:vars ([S Subsort] [Ss Subsort ?...])
      (cons S (subsorts Ss))
      (subsorts S Ss))
  (op (cons Op Ops) Ops)
  (=> #:vars ([O Op] [Os Op ?...])
      (cons O (ops Os))
      (ops O Os))
  (op (cons Equation Equations) Equations)
  (=> #:vars ([E Equation] [Es Equation ?...])
      (cons E (equations Es))
      (equations E Es))
  (op (cons Rule Rules) Rules)
  (=> #:vars ([R Rule] [Rs Rule ?...])
      (cons R (rules Rs))
      (rules R Rs))
  (op (cons Var Vars) Vars)
  (=> #:vars ([V Var] [Vs Var ?...])
      (cons V (vars Vs))
      (vars V Vs))
  (op (cons Term Args) Args)
  (op (cons Pattern PatternArgs) PatternArgs)
  (=> #:vars ([A Pattern] [As Pattern ?...])
      (cons A (args As))
      (args A As))
  ; A bit different - only one op is list-like. Perhaps there should
  ; be a subsort for fixed-arity domains.
  (op (cons Symbol Domain) Domain)
  (=> #:vars ([S Symbol] [Ss Symbol ?...])
      (cons S (domain Ss))
      (domain S Ss))

  ; Append for Rule (the only real list)
  (op (append Rule Rules) Rules)
  (=> #:vars ([NR Rule])
      (append NR (rules))
      (rules NR))
  (=> #:vars ([NR Rule] [R Rule] [Rs Rule ?...])
      (append NR (rules R Rs))
      (cons R (append NR (rules Rs))))

  ;
  ; rename-sort
  ;
  (op (rename-sort Node Symbol Symbol) Node)
  (=> #:vars ([Name Symbol] [Imports Imports]
              [Sorts Sorts] [Subsorts Subsorts]
              [Ops Ops] [Equations Equations] [Rules Rules]
              [S1 Symbol] [S2 Symbol])
      (rename-sort (node Name Imports Sorts Subsorts Ops Equations Rules) S1 S2)
      (node Name Imports
            (rename-sort Sorts S1 S2)
            (rename-sort Subsorts S1 S2)
            (rename-sort Ops S1 S2)
            (rename-sort Equations S1 S2)
            (rename-sort Rules S1 S2)))

  ;
  ; rename-sort: map-like application to lists
  ;
  (op (rename-sort Sorts Symbol Symbol) Sorts)
  (=> #:vars ([S1 Symbol] [S2 Symbol])
      (rename-sort (sorts) S1 S2)
      (sorts))
  (=> #:vars ([S Symbol] [S1 Symbol] [S2 Symbol] [Ss Symbol ?...])
      (rename-sort (sorts S Ss) S1 S2)
      (cons (rename-sort S S1 S2) (rename-sort (sorts Ss) S1 S2)))

  (op (rename-sort Subsorts Symbol Symbol) Subsorts)
  (=> #:vars ([S1 Symbol] [S2 Symbol])
      (rename-sort (subsorts) S1 S2)
      (subsorts))
  (=> #:vars ([S Subsort] [Ss Subsort ?...] [S1 Symbol] [S2 Symbol])
      (rename-sort (subsorts S Ss) S1 S2)
      (cons (rename-sort S S1 S2) (rename-sort (subsorts Ss) S1 S2)))

  (op (rename-sort Ops Symbol Symbol) Ops)
  (=> #:vars ([S1 Symbol] [S2 Symbol])
      (rename-sort (ops) S1 S2)
      (ops))
  (=> #:vars ([O Op] [Os Op ?...] [S1 Symbol] [S2 Symbol])
      (rename-sort (ops O Os) S1 S2)
      (cons (rename-sort O S1 S2) (rename-sort (ops Os) S1 S2)))

  (op (rename-sort Equations Symbol Symbol) Equations)
  (=> #:vars ([S1 Symbol] [S2 Symbol])
      (rename-sort (equations) S1 S2)
      (equations))
  (=> #:vars ([E Equation] [Es Equation ?...] [S1 Symbol] [S2 Symbol])
      (rename-sort (equations E Es) S1 S2)
      (cons (rename-sort E S1 S2) (rename-sort (equations Es) S1 S2)))

  (op (rename-sort Rules Symbol Symbol) Rules)
  (=> #:vars ([S1 Symbol] [S2 Symbol])
      (rename-sort (rules) S1 S2)
      (rules))
  (=> #:vars ([R Rule] [Rs Rule ?...] [S1 Symbol] [S2 Symbol])
      (rename-sort (rules R Rs) S1 S2)
      (cons (rename-sort R S1 S2) (rename-sort (rules Rs) S1 S2)))

  (op (rename-sort Vars Symbol Symbol) Vars)
  (=> #:vars ([S1 Symbol] [S2 Symbol])
      (rename-sort (vars) S1 S2)
      (vars))
  (=> #:vars ([V Var] [Vs Var ?...] [S1 Symbol] [S2 Symbol])
      (rename-sort (vars V Vs) S1 S2)
      (cons (rename-sort V S1 S2) (rename-sort (vars Vs) S1 S2)))

  ;
  ; rename-sort: the operations on the leaves of the tree
  ;
  (op (rename-sort Subsort Symbol Symbol) Subsort)
  (=> #:vars ([SA Symbol] [SB Symbol] [S1 Symbol] [S2 Symbol])
      (rename-sort (subsort SA SB) S1 S2)
      (subsort (rename-sort SA S1 S2) (rename-sort SB S1 S2)))
  
  (op (rename-sort Op Symbol Symbol) Op)
  (=> #:vars ([N Symbol] [D Domain] [S Symbol] [S1 Symbol] [S2 Symbol])
      (rename-sort (op N D S) S1 S2)
      (op N (rename-sort D S1 S2) (rename-sort S S1 S2)))
  (=> #:vars ([N Symbol] [D Domain] [S Symbol] [S1 Symbol] [S2 Symbol])
      (rename-sort (symop N D S) S1 S2)
      (symop N (rename-sort D S1 S2) (rename-sort S S1 S2)))

  (op (rename-sort Domain Symbol Symbol) Domain)
  (op (rename-sort VarLengthDomain Symbol Symbol) VarLengthDomain)
  (=> #:vars ([S1 Symbol] [S2 Symbol])
      (rename-sort (domain) S1 S2)
      (domain))
  (=> #:vars ([S Symbol] [Ss Symbol ?...] [S1 Symbol] [S2 Symbol])
      (rename-sort (domain S Ss) S1 S2)
      (cons (rename-sort S S1 S2) (rename-sort (domain Ss) S1 S2)))
  (=> #:vars ([S Symbol] [S1 Symbol] [S2 Symbol])
      (rename-sort (vl-domain S) S1 S2)
      (vl-domain (rename-sort S S1 S2)))

  (op (rename-sort Equation Symbol Symbol) Equation)
  (=> #:vars ([Vs Vars] [P1 Pattern] [P2 Pattern] [P3 Pattern]
              [S1 Symbol] [S2 Symbol])
      (rename-sort (= Vs P1 P2 P3) S1 S2)
      (= (rename-sort Vs S1 S2) P1 P2 P3))
  
  (op (rename-sort Rule Symbol Symbol) Rule)
  (=> #:vars ([Vs Vars] [P1 Pattern] [P2 Pattern] [P3 Pattern]
              [S1 Symbol] [S2 Symbol])
      (rename-sort (=> Vs P1 P2 P3) S1 S2)
      (=> (rename-sort Vs S1 S2) P1 P2 P3))
  
  (op (rename-sort Var Symbol Symbol) Var)
  (=> #:vars ([N Symbol] [S1 Symbol] [S2 Symbol])
      (rename-sort (var N S1) S1 S2)
      (var N S2))
  (=> #:vars ([N Symbol] [S1 Symbol] [S2 Symbol] [B Boolean])
      (rename-sort (svar N S1 B) S1 S2)
      (svar N S2 B))
  (=> #:vars ([V Var] [S1 Symbol] [S2 Symbol])
      (rename-sort V S1 S2)
      V)

  (op (rename-sort Symbol Symbol Symbol) Symbol)
  (=> #:vars ([S1 Symbol] [S2 Symbol]) (rename-sort S1 S1 S2) S2)
  (=> #:vars ([S Symbol] [S1 Symbol] [S2 Symbol]) (rename-sort S S1 S2) S)
  
  ;
  ; rename-op
  ;
  (op (rename-op Node Symbol Symbol) Node)
  (=> #:vars ([Name Symbol] [Imports Imports]
              [Sorts Sorts] [Subsorts Subsorts]
              [Ops Ops] [Equations Equations] [Rules Rules]
              [OS1 Symbol] [OS2 Symbol])
      (rename-op (node Name Imports Sorts Subsorts Ops Equations Rules) OS1 OS2)
      (node Name Imports Sorts Subsorts
            (rename-op Ops OS1 OS2)
            (rename-op Equations OS1 OS2)
            (rename-op Rules OS1 OS2)))

  ;
  ; rename-op: map-like application to lists and sets
  ;
  (op (rename-op Ops Symbol Symbol) Ops)
  (=> #:vars ([OS1 Symbol] [OS2 Symbol])
      (rename-op (ops) OS1 OS2)
      (ops))
  (=> #:vars ([O Op] [Os Op ?...] [OS1 Symbol] [OS2 Symbol])
      (rename-op (ops O Os) OS1 OS2)
      (cons (rename-op O OS1 OS2) (rename-op (ops Os) OS1 OS2)))

  (op (rename-op Equations Symbol Symbol) Equations)
  (=> #:vars ([OS1 Symbol] [OS2 Symbol])
      (rename-op (equations) OS1 OS2)
      (equations))
  (=> #:vars ([E Equation] [Es Equation ?...] [OS1 Symbol] [OS2 Symbol])
      (rename-op (equations E Es) OS1 OS2)
      (cons (rename-op E OS1 OS2) (rename-op (equations Es) OS1 OS2)))

  (op (rename-op Rules Symbol Symbol) Rules)
  (=> #:vars ([OS1 Symbol] [OS2 Symbol])
      (rename-op (rules) OS1 OS2)
      (rules))
  (=> #:vars ([R Rule] [Rs Rule ?...] [OS1 Symbol] [OS2 Symbol])
      (rename-op (rules R Rs) OS1 OS2)
      (cons (rename-op R OS1 OS2) (rename-op (rules Rs) OS1 OS2)))

  (op (rename-op PatternArgs Symbol Symbol) PatternArgs)
  (op (rename-op Args Symbol Symbol) Args)
  (=> #:vars ([OS1 Symbol] [OS2 Symbol])
      (rename-op (args) OS1 OS2)
      (args))
  (=> #:vars ([A Pattern] [As Pattern ?...] [OS1 Symbol] [OS2 Symbol])
      (rename-op (args A As) OS1 OS2)
      (cons (rename-op A OS1 OS2) (rename-op (args As) OS1 OS2)))

  ;
  ; rename-op: the operations on the leaves of the tree
  ;
  (op (rename-op Op Symbol Symbol) Op)
  (=> #:vars ([D Domain] [S Symbol] [OS1 Symbol] [OS2 Symbol])
      (rename-op (op OS1 D S) OS1 OS2)
      (op OS2 D S))
  (=> #:vars ([N Symbol] [D Domain] [S Symbol] [OS1 Symbol] [OS2 Symbol])
      (rename-op (op N D S) OS1 OS2)
      (op N D S))
  (=> #:vars ([D Domain] [S Symbol] [OS1 Symbol] [OS2 Symbol])
      (rename-op (symop OS1 D S) OS1 OS2)
      (symop OS2 D S))
  (=> #:vars ([N Symbol] [D Domain] [S Symbol] [OS1 Symbol] [OS2 Symbol])
      (rename-op (symop N D S) OS1 OS2)
      (symop N D S))

  (op (rename-op Equation Symbol Symbol) Equation)
  (=> #:vars ([Vs Vars] [P1 Pattern] [P2 Pattern] [P3 Pattern]
              [OS1 Symbol] [OS2 Symbol])
      (rename-op (= Vs P1 P2 P3) OS1 OS2)
      (= Vs (rename-op P1 OS1 OS2)
         (rename-op P2 OS1 OS2)
         (rename-op P3 OS1 OS2)))

  (op (rename-op Rule Symbol Symbol) Rule)
  (=> #:vars ([Vs Vars] [P1 Pattern] [P2 Pattern] [P3 Pattern]
              [OS1 Symbol] [OS2 Symbol])
      (rename-op (=> Vs P1 P2 P3) OS1 OS2)
      (=> Vs (rename-op P1 OS1 OS2)
          (rename-op P2 OS1 OS2)
          (rename-op P3 OS1 OS2)))

  (op (rename-op Pattern Symbol Symbol) Pattern)
  (op (rename-op Term Symbol Symbol) Term)
  (=> #:vars ([A PatternArgs] [OS1 Symbol] [OS2 Symbol])
      (rename-op (pattern OS1 A) OS1 OS2)
      (pattern OS2 (rename-op A OS1 OS2)))
  (=> #:vars ([OS Symbol] [A PatternArgs] [OS1 Symbol] [OS2 Symbol])
      (rename-op (pattern OS A) OS1 OS2)
      (pattern OS (rename-op A OS1 OS2)))
  (=> #:vars ([A Args] [OS1 Symbol] [OS2 Symbol])
      (rename-op (term OS1 A) OS1 OS2)
      (term OS2 (rename-op A OS1 OS2)))
  (=> #:vars ([OS Symbol] [A Args] [OS1 Symbol] [OS2 Symbol])
      (rename-op (term OS A) OS1 OS2)
      (term OS (rename-op A OS1 OS2)))
  (=> #:vars ([P Pattern] [OS1 Symbol] [OS2 Symbol])
      (rename-op P OS1 OS2)
      P)

  ;
  ; node-name
  ;
  (op (node-name Node Symbol) Node)
  (=> #:vars ([Name Symbol] [Imports Imports]
              [Sorts Sorts] [Subsorts Subsorts]
              [Ops Ops] [Equations Equations] [Rules Rules]
              [NN Symbol])
      (node-name (node Name Imports Sorts Subsorts Ops Equations Rules) NN)
      (node NN Imports Sorts Subsorts Ops Equations Rules))

  ;
  ; Add declaration
  ;
  (op (add Node Declaration) Node)
  (=> #:vars ([Name Symbol] [Imports Imports]
              [Sorts Sorts] [Subsorts Subsorts]
              [Ops Ops] [Equations Equations] [Rules Rules]
              [I Import])
      (add (node Name Imports Sorts Subsorts Ops Equations Rules) I)
      (node Name (cons I Imports) Sorts Subsorts Ops Equations Rules))

  (=> #:vars ([Name Symbol] [Imports Imports]
              [Sorts Sorts] [Subsorts Subsorts]
              [Ops Ops] [Equations Equations] [Rules Rules])
      (add (node Name Imports Sorts Subsorts Ops Equations Rules) (sorts))
      (node Name Imports Sorts Subsorts Ops Equations Rules))
  (=> #:vars ([Name Symbol] [Imports Imports]
              [Ss Symbol ?...] [Subsorts Subsorts]
              [Ops Ops] [Equations Equations] [Rules Rules]
              [NS Symbol] [NSs Symbol ?...])
      (add (node Name Imports (sorts Ss) Subsorts Ops Equations Rules)
           (sorts NS NSs))
      (add (node Name Imports (sorts NS Ss) Subsorts Ops Equations Rules)
           (sorts NSs)))

  (=> #:vars ([Name Symbol] [Imports Imports]
              [Sorts Sorts] [Subsorts Subsorts]
              [Ops Ops] [Equations Equations] [Rules Rules]
              [S Subsort])
      (add (node Name Imports Sorts Subsorts Ops Equations Rules) S)
      (node Name Imports Sorts (cons S Subsorts) Ops Equations Rules))

  (=> #:vars ([Name Symbol] [Imports Imports]
              [Sorts Sorts] [Subsorts Subsorts]
              [Ops Ops] [Equations Equations] [Rules Rules])
      (add (node Name Imports Sorts Subsorts Ops Equations Rules) (subsorts))
      (node Name Imports Sorts Subsorts Ops Equations Rules))
  (=> #:vars ([Name Symbol] [Imports Imports]
              [Sorts Sorts] [Ss Subsort ?...]
              [Ops Ops] [Equations Equations] [Rules Rules]
              [NS Subsort] [NSs Subsort ?...])
      (add (node Name Imports Sorts (subsorts Ss) Ops Equations Rules)
           (subsorts NS NSs))
      (add (node Name Imports Sorts (subsorts NS Ss) Ops Equations Rules)
           (subsorts NSs)))

  (=> #:vars ([Name Symbol] [Imports Imports]
              [Sorts Sorts] [Subsorts Subsorts]
              [Ops Ops] [Equations Equations] [Rules Rules]
              [NO Op])
      (add (node Name Imports Sorts Subsorts Ops Equations Rules) NO)
      (node Name Imports Sorts Subsorts (cons NO Ops) Equations Rules))

  (=> #:vars ([Name Symbol] [Imports Imports]
              [Sorts Sorts] [Subsorts Subsorts]
              [Ops Ops] [Equations Equations] [Rules Rules]
              [NE Equation])
      (add (node Name Imports Sorts Subsorts Ops Equations Rules) NE)
      (node Name Imports Sorts Subsorts Ops (cons NE Equations) Rules))

  (=> #:vars ([Name Symbol] [Imports Imports]
              [Sorts Sorts] [Subsorts Subsorts]
              [Ops Ops] [Equations Equations] [Rules Rules] 
              [NR Rule])
      (add (node Name Imports Sorts Subsorts Ops Equations Rules) NR)
      (node Name Imports Sorts Subsorts Ops Equations (append NR Rules)))

  ;
  ; Single-sort declaration
  ;
  (op (sort Symbol) Declaration)
  (=> #:var [S Symbol] (sort S) (sorts S))

  ;
  ; Transforms and their application
  ;
  (op (transforms Transform ...) Transforms)
  (op ($apply-transform Transform Node) Node)

  (op (node-name Symbol) Transform)
  (=> #:vars ([N Symbol] [M Node])
      ($apply-transform (node-name N) M)
      (node-name M N))

  (op (add Declaration) Transform)
  (=> #:vars ([D Declaration] [N Node])
      ($apply-transform (add D) N)
      (add N D))

  (op (rename-sort Symbol Symbol) Transform)
  (=> #:vars ([S1 Symbol] [S2 Symbol] [M Node])
      ($apply-transform (rename-sort S1 S2) M)
      (rename-sort M S1 S2))

  (op (rename-op Symbol Symbol) Transform)
  (=> #:vars ([O1 Symbol] [O2 Symbol] [M Node])
      ($apply-transform (rename-op O1 O2) M)
      (rename-op M O1 O2))

  ;
  ; Extended node
  ;
  (op (transformed-node Node Transforms) Node)

  (=> #:vars ([M Node] [T Transform])
      (transformed-node M (transforms T))
      ($apply-transform T M))
  (=> #:vars ([M Node] [T Transform] [Ts Transform ...])
      (transformed-node M (transforms T Ts))
      (transformed-node ($apply-transform T M) (transforms Ts)))
  
  ;
  ; Extended use and include
  ;
  (op (use Node Transforms) Import)
  (op (include Node Transforms) Import)

  (=> #:vars ([M Node] [T Transform])
      (use M (transforms T))
      (use ($apply-transform T M)))
  (=> #:vars ([M Node] [T Transform] [Ts Transform ...])
      (use M (transforms T Ts))
      (use ($apply-transform T M) (transforms Ts)))
  (=> #:vars ([M Node] [T Transform])
      (include M (transforms T))
      (include ($apply-transform T M)))
  (=> #:vars ([M Node] [T Transform] [Ts Transform ...])
      (include M (transforms T Ts))
      (include ($apply-transform T M) (transforms Ts))))
