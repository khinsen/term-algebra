#lang racket

(provide any-sort
         empty-sort-graph
         add-sort add-subsort merge-sort-graph
         is-subsort? has-sort?
         check-subsort-graph
         is-sort?
         kind
         subsorts
         least-common-sort)

(require graph)

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
(struct sort-graph (dag cc origins)
        #:transparent)

;
; Management of the sort graph
;
(define (empty-sort-graph)
  (let ([dag (unweighted-graph/directed '())]
        [cc (make-hash)]
        [origins (make-hash)])
    (sort-graph dag cc origins)))

(define (add-sort sort origin imports graph strict-checking)
  (let ([dag (sort-graph-dag graph)]
        [cc (sort-graph-cc graph)]
        [origins (sort-graph-origins graph)])
    (if (has-vertex? dag sort)
        (when strict-checking
          (error "sort already defined: " sort))
        (begin
          (add-vertex! dag sort)
          (hash-set! cc sort (set sort))
          (hash-set! origins sort origin))))
  graph)

(define (add-subsort sort1 sort2 origin imports graph strict-checking)

  (define (merge-connected-components cc sort1 sort2)
    (let ([new-cc-value (set-union (hash-ref cc sort1) (hash-ref cc sort2))])
      (for ([sort new-cc-value])
        (hash-set! cc sort new-cc-value))))

  (let ([dag (sort-graph-dag graph)]
        [cc (sort-graph-cc graph)]
        [origins (sort-graph-origins graph)])
    (cond
     [(equal? sort1 sort2)
      (when strict-checking
        (error "sorts are equal: " (cons sort1 sort2)))]
     [(or (equal? sort1 any-sort)
          (equal? sort2 any-sort))
      (error "special sort Any forbidden in subsort relations")]
     [(not (has-vertex? dag sort1))
      (error "undefined sort: " sort1)]
     [(not (has-vertex? dag sort2))
      (error "undefined sort: " sort2)]
     [(has-edge? dag sort2 sort1)
      (when strict-checking
        (error "subsort relation already defined: " (cons sort1 sort2)))]
     [(is-subsort? sort2 sort1 graph)
      (error "cyclic subsort dependence created by " (cons sort1 sort2))]
     [(and (hash-ref imports (hash-ref origins sort1) #f)
           (hash-ref imports (hash-ref origins sort2) #f))
      (error "both sorts from restricted import: " (cons sort1 sort2))]
     [else
      (begin
        (add-directed-edge! dag sort2 sort1)
        (merge-connected-components cc sort1 sort2)
        (hash-set! origins (list sort1 sort2) origin))]))
  graph)

(define (merge-sort-graph to-merge prior-imports graph)
  (let ([source-origins (sort-graph-origins to-merge)]
        [unrestricted (for/hash ([(hashcode _) (in-hash prior-imports)])
                        (values hashcode #f))])
    (for ([sort (in-vertices (sort-graph-dag to-merge))])
      (let ([origin (hash-ref source-origins sort)])
        (unless (hash-has-key? prior-imports origin)
          (add-sort sort origin unrestricted graph #f))))
    (for ([inv-subsort (in-edges (sort-graph-dag to-merge))])
      (let* ([subsort (reverse inv-subsort)]
             [origin (hash-ref source-origins subsort)])
        (unless (hash-has-key? prior-imports origin)
          (add-subsort (first subsort) (second subsort)
                       origin unrestricted graph #f))))
    graph))

(define (is-subsort? sort1 sort2 graph)
  (fewest-vertices-path (sort-graph-dag graph) sort2 sort1))

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
  (define-values (dist pred) (bfs (sort-graph-dag graph) sort))
  (set-add (for/set ([(sort psort) (in-hash pred)]
                     #:when psort)
             sort)
           sort))

(define (least-common-sort graph . sorts)
  ; In case of multiple equidistant common sorts, return an
  ; arbitrary one. The function is intended for use with argument
  ; sorts from an operator signature. Regularity guarantees that
  ; there is a unique least common sort.
  ; The implementation is not very efficient.
  (define-values (least-sort __)
    (for/fold ([least-sort #f]
               [min-distance #f])
              ([sort (in-set (kind (car sorts) graph))]
               #:when (andmap (λ (s) (is-subsort? s sort graph)) sorts))
      (let ([distance (length (fewest-vertices-path (sort-graph-dag graph)
                                                    sort (car sorts)))])
        (if (or (not min-distance) (< distance min-distance))
            (values sort distance)
            (values least-sort min-distance)))))
  least-sort)
