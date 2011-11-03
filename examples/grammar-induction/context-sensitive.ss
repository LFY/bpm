(import (printing)
        (grammar-induction)
        (bayes-program-merging)
        (scene-graphs)
        (program-likelihood)
        (util)
        (_srfi :1)
        )

;; Test data, as a list of S-expressions with elem, tr as constructors
;; (define data
;;   (list
;;      '(elem "root"
;;            (tr "right"
;;                (elem "node"
;;                      (tr "forward"
;;                          (elem "node"))))
;;            (tr "left"
;;                (elem "node"
;;                      (tr "forward"
;;                          (elem "node")))))                
;;        ))
;;

(define (chain n)
  (cond [(= 0 n) '(elem "node")]
        [else `(elem "node" (tr "forward" ,(chain (- n 1))))]))

(define (example n)
  `(elem "root"
         (tr "right" ,(chain n))
         (tr "left" ,(chain n))))

(define data
  (map example (iota 6)))

 (define program 
   (let* ([beam-width 10]
          [likelihood-weight 1.0]
          [prior-weight 2.0])
     (bpm data beam-width likelihood-weight prior-weight)))
 
;; (define rtg ;; inducing regular tree grammar
;;   (let* ([beam-width 10])
;;     (gi-bmm data beam-width 1.0 2.0)))
;; 
;; ;; program that takes sharing into account:
;; '((program
;;    ((abstraction F162 () (F161 (F160)))
;;      (abstraction F161 (V192)
;;        (elem "root" (tr "right" V192) (tr "left" V192)))
;;      (abstraction F160 ()
;;        ((lambda (V191) (elem "node" (tr "forward" V191)))
;;          (choose (elem "node") (F160) (F160) (F160) (F160)))))
;;    (lambda ()
;;      (choose (F161 (elem "node")) (F162) (F162) (F162)
;;        (F162) (F162))))
;;   -86.98199232851043)
;; 
;; ;; vs grammar:
;; '((program
;;    ((abstraction F82 ()
;;       (choose (elem "node" (tr "forward" (F82)))
;;         (elem "node")))
;;      (abstraction F77 ()
;;        (choose
;;          (elem "root" (tr "right" (F82)) (tr "left" (F82)))
;;          (elem "root" (tr "right" (F82)) (tr "left" (F82)))
;;          (elem "root" (tr "right" (F82)) (tr "left" (F82)))
;;          (elem "root" (tr "right" (F82)) (tr "left" (F82)))
;;          (elem "root" (tr "right" (F82)) (tr "left" (F82)))
;;          (elem "root" (tr "right" (F82)) (tr "left" (F82))))))
;;    (lambda () (choose (F77) (F77) (F77) (F77) (F77) (F77))))
;;   -159.1121815835177)
;; 
