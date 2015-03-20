#lang racket

(provide module++ define-module++
         unchecked-module++
         define-unchecked-module++ )

(require term-algebra/term-syntax
         term-algebra/module-syntax
         term-algebra/library/module-transforms
         (prefix-in meta: term-algebra/meta)
         (for-syntax term-algebra/module-syntax)
         (for-syntax syntax/parse))

(define (prepare-import module . transforms)
  (cond
    [(empty? transforms) (meta:module-hashcode module)]
    [else
     (meta:module-hashcode
      (meta:reduce-vterm
       (term module-transforms
             (transformed-module ,module (transforms ,@transforms)))))]))

(begin-for-syntax

  (define-syntax-class module-transform
    #:description "module transform specification"
    #:attributes (transform)
    (pattern ((~datum module-name) name:id)
             #:with transform
             #'(term module-transforms
                     (module-name (quote name))))
    (pattern ((~datum add-import) import-decl:extended-import)
             #:with transform
             #'(term module-transforms
                     (add-import ,@import-decl.imports)))
    (pattern ((~datum rename-sort) name1:id name2:id)
             #:with transform
             #'(term module-transforms
                     (rename-sort (quote name1) (quote name2))))
    (pattern ((~datum rename-op) name1:id name2:id)
             #:with transform
             #'(term module-transforms
                     (rename-op (quote name1) (quote name2)))))
  
  (define-syntax-class extended-import
    #:description "import declaration"
    #:datum-literals (use include)
    #:attributes (imports sorts subsorts ops rules)
    (pattern (use module:id
                  (~optional (~seq #:transforms mt:module-transform ...) 
                             #:defaults ([(mt.transform 1) null])) ...)
             #:with imports
             #'(list (term m-module
                           (use ,(prepare-import module mt.transform ...))))
             #:with sorts #'(list)
             #:with subsorts #'(list)
             #:with ops #'(list)
             #:with rules #'(list))
    (pattern (include module:id
                      (~optional (~seq #:transforms mt:module-transform ...) 
                                 #:defaults ([(mt.transform 1) null])) ...)
             #:with imports
             #'(list (term m-module
                           (include ,(prepare-import module mt.transform ...))))
             #:with sorts #'(list)
             #:with subsorts #'(list)
             #:with ops #'(list)
             #:with rules #'(list)))
  
  (define-syntax-class decl
    #:description "declaration in a module"
    #:attributes (imports sorts subsorts ops rules)
    (pattern import-decl:extended-import
             #:with imports #'import-decl.imports
             #:with sorts #'(list)
             #:with subsorts #'(list)
             #:with ops #'(list)
             #:with rules #'(list))
    (pattern sort-decl:sort
             #:with imports #'(list)
             #:with sorts #'sort-decl.sorts
             #:with subsorts #'sort-decl.subsorts
             #:with ops #'(list)
             #:with rules #'(list))
    (pattern op-decl:operator
             #:with imports #'(list)
             #:with sorts #'(list)
             #:with subsorts #'(list)
             #:with ops #'op-decl.ops
             #:with rules #'op-decl.rules)))

(define-syntax (unchecked-module++ stx)
  (syntax-parse stx
    [(_ module-name:id
        declaration:decl ...)
     #'(term m-module
             (module (quote module-name)
               (imports ,@(append declaration.imports ...))
               (sorts ,@(append declaration.sorts ...))
               (subsorts ,@(append declaration.subsorts ...))
               (ops ,@(append declaration.ops ...))
               (rules ,@(append declaration.rules ...))))]))

(define-syntax (define-unchecked-module++ stx)
  (syntax-parse stx
    [(_ module-name:id decl ...)
     #'(define module-name (unchecked-module++ module-name decl ...))]))

(define-syntax (module++ stx)
  (syntax-parse stx
    [(_ arg ...)
     #'(meta:check-module (unchecked-module++ arg ...))]))

(define-syntax (define-module++ stx)
  (syntax-parse stx
    [(_ module-name:id decl ...)
     #'(define module-name (module++ module-name decl ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require term-algebra/api
           term-algebra/library/list)
  (define-module++ test1
    (include list
             #:transforms (add-import (use builtin:string))
                          (rename-sort Element String)
                          (rename-sort List StringList)
                          (rename-sort NonEmptyList NEStringList)
                          (rename-op list string-list))
    (op bar NEStringList)
    (=-> bar (string-list "a" "b" "c")))
  (meta:reduce-vterm (term test1 (tail bar))))
