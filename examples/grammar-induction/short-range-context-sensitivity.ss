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

(define rtg ;; inducing regular tree grammar
  (let* ([beam-width 10])
    (gi-bmm data beam-width 1.0 2.0)))

(pretty-print program)
(pretty-print rtg)

;; In this case, both stochastic lambda calculus and regular tree grammar are
;; capable of detecting the context-specific dependency

'((program
   ((abstraction F150 (V290 V291)
      (elem "root" (tr "red_chain" V291)
        (tr "blue_chain" V290)))
     (abstraction F149 () (F150 (F148) (F147)))
     (abstraction F148 () (F146 "blue"))
     (abstraction F147 () (F146 "red"))
     (abstraction F146 (V289)
       ((lambda (V288) (elem V289 (tr "forward" V288)))
         (choose (elem "red") (elem "blue") (F148) (F147)
           (F148) (F147))))
     (abstraction F145 () (F149)))
   (lambda ()
     (choose (F150 (elem "blue") (elem "red")) (F149) (F145)
       (F145) (F145) (F145))))
  -142.59320771884057)

'(program
  ((abstraction F82 ()
     (choose (elem "blue" (tr "forward" (F82)))
       (elem "blue")))
    (abstraction F77 ()
      (choose (elem "red") (elem "red" (tr "forward" (F77)))))
    (abstraction F72 ()
      (choose
        (elem "root" (tr "red_chain" (F77))
          (tr "blue_chain" (F82)))
        (elem "root" (tr "red_chain" (F77))
          (tr "blue_chain" (F82)))
        (elem "root" (tr "red_chain" (F77))
          (tr "blue_chain" (F82)))
        (elem "root" (tr "red_chain" (F77))
          (tr "blue_chain" (F82)))
        (elem "root" (tr "red_chain" (F77))
          (tr "blue_chain" (F82)))
        (elem "root" (tr "red_chain" (F77))
          (tr "blue_chain" (F82))))))
  (lambda () (choose (F72) (F72) (F72) (F72) (F72) (F72))))
