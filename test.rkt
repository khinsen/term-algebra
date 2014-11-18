#lang racket

(require term-algebra/client
         (only-in term-algebra/terms reduce))


(module truth term-algebra
  (define-op true)
  (define-op false))

(use-module truth (submod "." truth))

;; (module boolean term-algebra
;;   (define-op true)
;;   (define-op false)
  
;;   (define-op (not x))
;;   (define-op (and x y))
;;   (define-op (or x y))
    
;;   (=-> (not true) false)
;;   (=-> (not false) true)

;;   (define-vars X Y)

;;   (=-> (not (not X)) X)

;;   (=-> (and true true) true)
;;   (=-> (and false X) false)
;;   (=-> (and X false) false)
;;   (=-> (not (and X Y)) (or (not X) (not Y)))
      
;;   (=-> (or false false) false)
;;   (=-> (or true X) true)
;;   (=-> (or X true) true)
;;   (=-> (not (or X Y)) (and (not X) (not Y))))

;; (use-module boolean (submod "." boolean))

(syntax-source #'hash)
(syntax-line #'hash)
(syntax-column #'hash)
(syntax-position #'hash)
(syntax-span #'hash)

;; (define (test-for term)
;;   (display term)
;;   (display " -> ")
;;   (displayln (reduce term)))

;; (test-for (term boolean (and true true)))
