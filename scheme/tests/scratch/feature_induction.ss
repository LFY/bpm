
(define (induce-one-step background data current-hypothesis)
  (argmax (lambda (x) data-hyp->posterior data x) 
          (get-refinements background data current-hypothesis)))

(define (feature-induction background data current-hypothesis stop-criterion)
  (let* ([new-hypothesis (induce-one-step background data current-hypothesis)])
    (if (stop-criterion current-hypothesis new-hypothesis) 
      current-hypothesis
      (feature-induction background data (first new-hyp-score) stop-criterion))))

