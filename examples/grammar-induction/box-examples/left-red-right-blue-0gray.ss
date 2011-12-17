;; A root node, 0 gray nodes, and a chain of red/blue on the left and right.

(define (chain elt-type n next-chain)
  (cond [(= 0 n) next-chain]
        [else `(elem ,elt-type 
                     (tr "forward" 
                         ,(chain elt-type (- n 1) next-chain)))]))

(define (example n)
  `(elem "root"
         (tr "left" ,(chain "gray" 0 (chain "red" n '(elem "red"))))
         (tr "right" ,(chain "gray" 0 (chain "blue" n '(elem "blue"))))))

(define data
  (map example (iota 3)))
