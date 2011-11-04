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
  (map example (iota 2)))

;;(define program ;; inducing program
  ;;(let* ([beam-width 10])
    ;;(bpm data beam-width 1.0 2.0)))

(define rtg ;; inducing regular tree grammar
  (let* ([beam-width 10])
    (gi-bmm data beam-width 1.0 2.0)))

(pretty-print program)

;; Can't detect long-distance context-sensitive dependence, because of how
;; recursion moves are inferred

'((program
   ((abstraction F260 () (F259 (F258) (F258)))
     (abstraction F259 (V312 V313)
       (elem "root" (tr "red_chain" V313)
         (tr "blue_chain" V312)))
     (abstraction F258 ()
       ((lambda (V311) (elem "other" (tr "forward" V311)))
         (choose (elem "red") (elem "blue") (F258) (F258)
           (F258) (F258) (F258) (F258) (F258) (F258)))))
   (lambda ()
     (choose (F259 (elem "blue") (elem "red")) (F260) (F260)
       (F260) (F260) (F260))))
  -122.19208920942248)

;; Neither can the grammar
'((program
    ((abstraction F81 ()
                  (choose (elem "other" (tr "forward" (F24)))
                          (elem "other" (tr "forward" (F29)))
                          (elem "other" (tr "forward" (F81)))))
     (abstraction F72 ()
                  (choose
                    (elem "root" (tr "red_chain" (F81))
                          (tr "blue_chain" (F81)))
                    (elem "root" (tr "red_chain" (F81))
                          (tr "blue_chain" (F81)))
                    (elem "root" (tr "red_chain" (F81))
                          (tr "blue_chain" (F81)))
                    (elem "root" (tr "red_chain" (F81))
                          (tr "blue_chain" (F81)))
                    (elem "root" (tr "red_chain" (F24))
                          (tr "blue_chain" (F29)))
                    (elem "root" (tr "red_chain" (F81))
                          (tr "blue_chain" (F81)))))
     (abstraction F24 () (elem "red"))
     (abstraction F29 () (elem "blue")))
    (lambda () (choose (F72) (F72) (F72) (F72) (F72) (F72))))
  -193.6617359132411)

