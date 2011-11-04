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

(define (chain-other n rest)
  (cond [(= 0 n) rest]
        [else `(elem "other"
                     (tr "forward"
                         ,(chain-other (- n 1) rest)))]))
(define (example n)
  `(elem "root"
         (tr "red_chain" ,(chain-other 1 (chain "red" n)))
         (tr "blue_chain" ,(chain-other 1 (chain "blue" n)))))

(define data
  (map example (iota 6)))

;; (define program ;; inducing program
  ;; (let* ([beam-width 10])
    ;; (bpm data beam-width 1.0 2.0)))

;; (pretty-print program)
(define rtg ;; inducing regular tree grammar
  (let* ([beam-width 10])
    (gi-bmm data beam-width 1.0 1.0)))
(pretty-print rtg)

;; the induced grammar: 1:2 likelihood:prior weigting

'(program
  ((abstraction F105 ()
     (choose (elem "red" (tr "forward" (F105))) (elem "red")))
    (abstraction F100 ()
      (choose (elem "blue")
        (elem "blue" (tr "forward" (F100)))))
    (abstraction F99 ()
      (choose (elem "other" (tr "forward" (F100)))
        (elem "other" (tr "forward" (F105)))
        (elem "other" (tr "forward" (F100)))
        (elem "other" (tr "forward" (F105)))
        (elem "other" (tr "forward" (F105)))
        (elem "other" (tr "forward" (F105)))
        (elem "other" (tr "forward" (F105)))
        (elem "other" (tr "forward" (F105)))))
    (abstraction F84 ()
      (choose
        (elem "root" (tr "red_chain" (F99))
          (tr "blue_chain" (F99)))
        (elem "root" (tr "red_chain" (F99))
          (tr "blue_chain" (F99)))
        (elem "root" (tr "red_chain" (F99))
          (tr "blue_chain" (F99)))
        (elem "root" (tr "red_chain" (F99))
          (tr "blue_chain" (F99)))
        (elem "root" (tr "red_chain" (F99))
          (tr "blue_chain" (F99)))
        (elem "root" (tr "red_chain" (F99))
          (tr "blue_chain" (F99))))))
  (lambda () (choose (F84) (F84) (F84) (F84) (F84) (F84))))

;; but somewhere along the way, it passes by this more-specific grammar that
;; captures the context sensitivty we want:

'((program
   ((abstraction F102 ()
      (choose (elem "red" (tr "forward" (F102)))
        (elem "red")))
     (abstraction F99 ()
       (choose (elem "other" (tr "forward" (F102)))
         (elem "other" (tr "forward" (F102)))
         (elem "other" (tr "forward" (F50)))
         (elem "other" (tr "forward" (F102)))
         (elem "other" (tr "forward" (F78)))
         (elem "other" (tr "forward" (F102)))))
     (abstraction F94 () (elem "other" (tr "forward" (F93))))
     (abstraction F93 ()
       (choose (elem "blue")
         (elem "blue" (tr "forward" (F93)))))
     (abstraction F84 ()
       (choose
         (elem "root" (tr "red_chain" (F99)) ;; F99: red chains
           (tr "blue_chain" (F94))) ;; F94: blue chains
         (elem "root" (tr "red_chain" (F99))
           (tr "blue_chain" (F94)))
         (elem "root" (tr "red_chain" (F99))
           (tr "blue_chain" (F94)))
         (elem "root" (tr "red_chain" (F99))
           (tr "blue_chain" (F94)))
         (elem "root" (tr "red_chain" (F99))
           (tr "blue_chain" (F94)))
         (elem "root" (tr "red_chain" (F99))
           (tr "blue_chain" (F94)))))
     (abstraction F78 () (elem "red" (tr "forward" (F102))))
     (abstraction F50 () (elem "red" (tr "forward" (F78)))))
   (lambda () (choose (F84) (F84) (F84) (F84) (F84) (F84))))
  -149.4743441965619)
