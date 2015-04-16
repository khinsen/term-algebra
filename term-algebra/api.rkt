#lang racket

(provide define-node node
         define-unchecked-node unchecked-node
         term meta-term
         meta-up meta-down
         reduce
         ; builtin nodes:
         builtin:any
         builtin:truth builtin:equality
         builtin:string builtin:symbol
         builtin:natural builtin:integer builtin:rational
         builtin:term builtin:node)

(require (except-in term-algebra/basic-api
                    define-node node
                    define-unchecked-node unchecked-node)
         term-algebra/node-syntax)
