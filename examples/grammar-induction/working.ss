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
         (tr "red_chain" ,(chain-other 0 (chain "red" n)))
         (tr "blue_chain" ,(chain-other 0 (chain "blue" n)))))

(define data
  (map example (iota 7)))

(define program ;; inducing program
  (let* ([beam-width 10])
    (bpm data beam-width 1.0 2.5)))
(pretty-print program)

;; 1:2
;; captures med-range dependency but not recursive structure?
'((program
   ((abstraction F236 (V523) (F229 V523 "red"))
     (abstraction F235 () (elem "red"))
     (abstraction F234 () (elem "blue"))
     (abstraction F233 (V522) (F229 V522 "blue"))
     (abstraction F232 () (F236 (F235)))
     (abstraction F231 () (F233 (F234)))
     (abstraction F230 (V520 V521)
       (elem "root" (tr "red_chain" (F229 V521 "other"))
         (tr "blue_chain" (F229 V520 "other"))))
     (abstraction F229 (V518 V519)
       (elem V519 (tr "forward" V518)))
     (abstraction F228 (V516 V517)
       (F230 (F233 (F233 V516)) (F236 (F236 V517)))))
   (lambda ()
     (choose (F230 (F234) (F235)) (F230 (F231) (F232))
       (F228 (F234) (F235)) (F228 (F231) (F232))
       (F228 (F233 (F231)) (F236 (F232)))
       (F228 (F233 (F233 (F231))) (F236 (F236 (F232)))))))
  -156.75055681536833)

;; 1:2.5 
'(program
  ((abstraction F161 (V337 V338)
     (elem "root" (tr "red_chain" V338)
       (tr "blue_chain" V337)))
    (abstraction F160 () (elem "blue"))
    (abstraction F159 () (elem "red"))
    (abstraction F158 ()
      ((lambda (V335) (F153 V335 "blue"))
        (choose (F155) (F158) (F160))))
    (abstraction F157 ()
      ((lambda (V336) (F153 V336 "red"))
        (choose (F156) (F157) (F159))))
    (abstraction F156 () (F157))
    (abstraction F155 () (F158))
    (abstraction F154 () (F161 (F158) (F157)))
    (abstraction F153 (V333 V334)
      (elem V334 (tr "forward" V333)))
    (abstraction F152 () (F154)))
  (lambda ()
    (choose (F161 (F160) (F159)) (F154) (F154) (F152) (F152)
      (F152) (F152))))
