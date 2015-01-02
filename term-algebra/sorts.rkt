#lang racket

(provide empty-sort-graph
         add-sort add-subsort merge-sort-graph
         is-subsort? has-sort?
         check-subsort-graph
         is-sort?
         any-sort)

(require (prefix-in terms: term-algebra/terms)
         graph)

;
; A special sort that is compatible with everything
; but may not be used explicitly in a sort graph.
;
(define any-sort (terms:sort 'Any))

;
; Management of the sort graph
;
(define (empty-sort-graph)
  (unweighted-graph/directed '()))

(define (add-sort sort graph)
  (if (has-vertex? graph sort)
      (error "sort already defined: " sort)
      (add-vertex! graph sort))
  graph)

(define (add-subsort sort1 sort2 graph)
  (cond
   [(equal? sort1 sort2)
    (error "sorts are equal: " (cons sort1 sort2))]
   [(or (equal? sort1 any-sort)
        (equal? sort2 any-sort))
    (error "special sort Any forbidden in subsort relations")]
   [(not (has-vertex? graph sort1))
    (error "undefined sort: " sort1)]
   [(not (has-vertex? graph sort2))
    (error "undefined sort: " sort2)]
   [(has-edge? graph sort1 sort2)
    (error "subsort relation already defined: " (cons sort1 sort2))]
   [(is-subsort? sort2 sort1 graph)
    (error "cyclic subsort dependence created by " (cons sort1 sort2))]
   [else (add-directed-edge! graph sort1 sort2)])
  graph)

(define (merge-sort-graph to-merge graph)
  (for ([sort (in-vertices to-merge)])
    (add-sort sort graph))
  (for ([subsort (in-edges to-merge)])
    (add-subsort (first subsort) (second subsort) graph))
  graph)

(define (is-subsort? sort1 sort2 graph)
  (fewest-vertices-path graph sort1 sort2))

(define (has-sort? sort graph)
  (has-vertex? graph sort))

(define (check-subsort-graph graph)
  (when (not (dag? graph))
    (error "subsort graph is not acyclic")))

;
; Higher-level sort operations
;
(define (is-sort? sort target-sort graph)
  (or (equal? sort target-sort)
      (equal? target-sort any-sort)
      (is-subsort? sort target-sort graph)))
