;; A root node, that ends in a red node on the left and a blue node on the right.

(define (chain elt-type n next-chain)
  (cond [(= 0 n) next-chain]
        [else `(elem ,elt-type 
                     (tr "forward" 
                         ,(chain elt-type (- n 1) next-chain)))]))

(define (example n)
  `(elem "root"
         (tr "left" ,(chain "gray" n (chain "red" 0 '(elem "red"))))
         (tr "right" ,(chain "gray" n (chain "blue" 0 '(elem "blue"))))))

(define data
  (map example (iota 3)))
