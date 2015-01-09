#lang racket

(provide (struct-out signature))

(require (prefix-in sorts: term-algebra/sorts)
         (prefix-in operators: term-algebra/operators))

; The data structure for a module signature
;
(struct signature (sorts ops)
        #:transparent)

