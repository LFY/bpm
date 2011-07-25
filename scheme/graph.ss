(library (graph)
         (export one-ring
                 next-fringe
                 all-reachable
                 connected-components)
         (import (rnrs)
                 (_srfi :1)
                 (util))

         ; Some graph algorithms

         ; 1-ring of an edge
         (define (one-ring edge edges)
           (define (touch? e2)
             (or (contains? (first edge) e2)
                 (contains? (second edge) e2)))
           (filter touch? edges))

         ; Fringe: one-ring of multiple edges
         (define (next-fringe fringe edges)
           (list-subtract (delete-duplicates (concatenate (map (lambda (e) (one-ring e edges)) fringe)))
                          fringe))

         ; Get all edges reachable from an edge
         (define (all-reachable edge edges)
           (define (reachable-loop res fringe rem-edges)
             (cond [(null? fringe) res]
                   [else (reachable-loop (append fringe res)
                                         (next-fringe fringe rem-edges)
                                         (list-subtract rem-edges fringe))]))
           (reachable-loop '() (one-ring edge edges) edges))

         ; Get all connected components of a graph
         (define (connected-components edges)
           (define (component-loop res rem-edges)
             (cond [(null? rem-edges) res]
                   [else (let* ([component (all-reachable (first rem-edges) rem-edges)]
                                [remaining (list-subtract rem-edges component)])
                           (component-loop (cons component res) remaining))]))
           (component-loop '() edges))


         ; Spanning tree
         
         ; Topological sort
         
         )
