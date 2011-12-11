(import (printing)
        (grammar-induction)
        (bayes-program-merging)
        (scene-graphs)
        (program-likelihood)
        (util)
        (_srfi :1)
        )

;; context-sensitivity w/ greater distance

(define (chain elt-type n next-chain)
  (cond [(= 0 n) next-chain]
        [else `(elem ,elt-type 
                     (tr "forward" 
                         ,(chain elt-type (- n 1) next-chain)))]))

(define (example n)
  `(elem "root"
         (tr "left" ,(chain "gray" 2 (chain "red" n '(elem "red"))))
         (tr "right" ,(chain "gray" 2 (chain "blue" n '(elem "blue"))))))

(define data
  (map example (iota 3)))

(define rtg 
    (gi-bmm data 10 1.0 1.0 1.0 2))


