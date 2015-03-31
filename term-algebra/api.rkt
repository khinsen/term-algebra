#lang racket

(provide define-section section
         define-unchecked-section unchecked-section
         term meta-term
         meta-up meta-down
         m-term m-module
         reduce
         ; builtin modules:
         builtin:any
         builtin:truth builtin:equality
         builtin:string builtin:symbol
         builtin:natural builtin:integer builtin:rational)

(require (except-in term-algebra/basic-api
                    define-module module
                    define-unchecked-module unchecked-module)
         term-algebra/module-syntax)
