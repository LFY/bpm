(import (printing)
        (grammar-induction)
        (bayes-program-merging)
        (scene-graphs)
        (program-likelihood)
        (util)
        (_srfi :1)
        )

;; context-sensitivity w/ greater distance

(define (chain color n)
  (cond [(= 0 n) `(elem ,color)]
        [else `(elem "other" (tr "forward" ,(chain color (- n 1))))]))

(define (example n)
  `(elem "root"
         (tr "red_chain" ,(chain "red" n))
         (tr "blue_chain" ,(chain "blue" n))))

(define data
  (map example (iota 3)))

(define rtg ;; inducing regular tree grammar
  (let* ([beam-width 10])
    (gi-bmm data beam-width 1.0 2.0 1.0 2)))

