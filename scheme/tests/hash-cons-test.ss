(import (hash-cons)
        (printing)
        (_srfi :1)
        (util)
        (combinations)
        (inverse-inline))

(define my-data (condense-program '(program ()
  (lambda ()
    (choose
      (node (data)
            (node (data (x 0) (y 1)))
            (node (data (x 2) (y 4))))
      )))))

;; (define my-data '(node (translate 1) (translate 1)))

(define my-map (sexpr->dag my-data))

(pretty-print (bimap-format my-map))
(pretty-print (dag->sexpr my-map))

;; (print "Old algorithm:")
;; (pretty-print (length (get-all-subexpr-pairs-old my-data)))

;; (print "New algorithm:")
;; (print my-data)
;; (pretty-print (get-all-subexpr-pairs my-data))

(print "Nested list:")
(define nested-list-dag (sexpr->dag '(a (b (c d)) ((b ((c d))))))) 
(print (bimap-format nested-list-dag))
(print (dag->sexpr nested-list-dag))
