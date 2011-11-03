(import (printing)
        (grammar-induction)
        (bayes-program-merging)
        (scene-graphs)
        (program-likelihood)
        (util)
        (_srfi :1)
        )

(define (chain color n)
  (cond [(= 0 n) `(elem ,color)]
        [else `(elem ,color (tr "forward" ,(chain color (- n 1))))]))

(define (example n)
  `(elem "root"
         (tr "red_chain" ,(chain "red" n))
         (tr "blue_chain" ,(chain "blue" n))))

(define data
  (map example (iota 6)))

(define program ;; inducing program
  (let* ([beam-width 10])
    (bpm data beam-width 1.0 2.0)))

;;(define rtg ;; inducing regular tree grammar
  ;;(let* ([beam-width 10])
    ;;(gi-bmm data beam-width 1.0 2.0)))
