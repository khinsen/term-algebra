#lang racket

(provide any-sort
         empty-sort-graph
         add-sort add-subsort merge-sort-graph
         is-subsort? has-sort?
         check-subsort-graph
         is-sort?
         kind
         subsorts)

(require (prefix-in terms: term-algebra/terms)
         graph)

;
; A special sort that is compatible with everything
; but may not be used explicitly in a sort graph.
;
(define any-sort 'Any)

;
; The sort graph data structure consists of the DAG itself plus a hash
; mapping each sort to its kind. The kind is defined as the connected
; component to which the sort belongs, and is represented by a set of
; all the sorts that belong to it.
;
(struct sort-graph (dag cc)
        #:transparent)

;
; Management of the sort graph
;
(define (empty-sort-graph)
  (let ([dag (unweighted-graph/directed '())]
        [cc (make-hash)])
    (sort-graph dag cc)))

(define (add-sort sort graph)
  (let ([dag (sort-graph-dag graph)]
        [cc (sort-graph-cc graph)])
    (if (has-vertex? dag sort)
        (error "sort already defined: " sort)
        (begin
          (add-vertex! dag sort)
          (hash-set! cc sort (set sort)))))
  graph)

(define (add-subsort sort1 sort2 graph)
  
  (define (merge-connected-components cc sort1 sort2)
    (let ([new-cc-value (set-union (hash-ref cc sort1) (hash-ref cc sort2))])
      (for ([sort new-cc-value])
        (hash-set! cc sort new-cc-value))))
  
  (let ([dag (sort-graph-dag graph)]
        [cc (sort-graph-cc graph)])
    (cond
     [(equal? sort1 sort2)
      (error "sorts are equal: " (cons sort1 sort2))]
     [(or (equal? sort1 any-sort)
          (equal? sort2 any-sort))
      (error "special sort Any forbidden in subsort relations")]
     [(not (has-vertex? dag sort1))
      (error "undefined sort: " sort1)]
     [(not (has-vertex? dag sort2))
      (error "undefined sort: " sort2)]
     [(has-edge? dag sort1 sort2)
      (error "subsort relation already defined: " (cons sort1 sort2))]
     [(is-subsort? sort2 sort1 graph)
      (error "cyclic subsort dependence created by " (cons sort1 sort2))]
     [else
      (begin
        (add-directed-edge! dag sort1 sort2)
        (merge-connected-components cc sort1 sort2))]))
  graph)

(define (merge-sort-graph to-merge graph)
  (for ([sort (in-vertices (sort-graph-dag to-merge))])
    (add-sort sort graph))
  (for ([subsort (in-edges (sort-graph-dag to-merge))])
    (add-subsort (first subsort) (second subsort) graph))
  graph)

(define (is-subsort? sort1 sort2 graph)
  (fewest-vertices-path (sort-graph-dag graph) sort1 sort2))

(define (has-sort? sort graph)
  (has-vertex? (sort-graph-dag graph) sort))

(define (check-subsort-graph graph)
  (when (not (dag? (sort-graph-dag graph)))
    (error "subsort graph is not acyclic")))

;
; Higher-level sort operations
;
(define (is-sort? sort target-sort graph)
  (or (equal? sort target-sort)
      (equal? target-sort any-sort)
      (is-subsort? sort target-sort graph)))

(define (kind sort graph)
  (hash-ref (sort-graph-cc graph) sort))

(define (subsorts sort graph)
  ; This is probably very inefficient!
  (let ([tc (transitive-closure (sort-graph-dag graph))])
    (for/set ([s (kind sort graph)]
              #:when (hash-ref tc (list s sort)))
      s)))
