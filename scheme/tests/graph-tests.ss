(import (graph)
        (printing)
        (util)
        (_srfi :1))

(define graph1 '((0 1)
                 (1 2)
                 (2 3)
                 (3 0)))

(define graph2 '((4 5)
                 (5 6)
                 (6 7)
                 (7 4)))

(define graph3 (append graph1 graph2))

(pretty-print (one-ring '(0 1) graph1))
(pretty-print (all-reachable '(0 1) graph1))
(pretty-print (one-ring '(4 5) graph1))
(pretty-print (all-reachable '(4 5) graph1))

(pretty-print (all-reachable '(0 1) graph3))
(pretty-print (all-reachable '(4 5) graph3))

(pretty-print (connected-components graph3))
