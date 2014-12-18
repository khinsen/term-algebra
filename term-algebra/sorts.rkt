#lang racket

(provide empty-sort-graph
         add-sort add-subsort merge-sort-graph
         is-subsort?
         check-subsort-graph)

(require (prefix-in terms: term-algebra/terms)
         graph)

(define (empty-sort-graph)
  (unweighted-graph/directed '()))

(define (add-sort sort graph)
  (if (has-vertex? graph sort)
      (error "sort already defined: " sort)
      (add-vertex! graph sort))
  graph)

(define (add-subsort sort1 sort2 graph)
  (cond
   [(not (has-vertex? graph sort1))
    (error "sort undefined: " sort1)]
   [(not (has-vertex? graph sort2))
    (error "sort undefined: " sort2)]
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

(define (check-subsort-graph graph)
  (when (not (dag? graph))
    (error "subsort graph is not acyclic")))
