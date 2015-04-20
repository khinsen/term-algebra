#lang racket

(provide define-node node
         define-unchecked-node unchecked-node
         term meta-term
         meta-up meta-down
         (rename-out [meta:reduce-vterm reduce]
                     [meta:in-vterm-reduction in-reduction])
         ; builtin nodes:
         builtin:any
         builtin:truth builtin:equality
         builtin:string builtin:symbol
         builtin:natural builtin:integer builtin:rational
         (rename-out [n-term builtin:term]
                     [n-node builtin:node]))

(require term-algebra/term-syntax
         term-algebra/basic-node-syntax
         (only-in term-algebra/meta meta-up meta-down)
         (prefix-in terms: term-algebra/terms)
         (prefix-in nodes: term-algebra/nodes)
         (prefix-in meta: term-algebra/meta))

; Builtin nodes

(define builtin:any
  (meta:check-node
   (term n-node (builtin-node 'any))))

(define builtin:truth
  (meta:check-node
   (term n-node (builtin-node 'truth))))

(define builtin:equality
  (meta:check-node
   (term n-node (builtin-node 'equality))))

(define builtin:string
  (meta:check-node
   (term n-node (builtin-node 'string))))

(define builtin:symbol
  (meta:check-node
   (term n-node (builtin-node 'symbol))))

(define builtin:natural
  (meta:check-node
   (term n-node (builtin-node 'natural))))

(define builtin:integer
  (meta:check-node
   (term n-node (builtin-node 'integer))))

(define builtin:rational
  (meta:check-node
   (term n-node (builtin-node 'rational))))
