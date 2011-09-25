(define (feature-induction-gen background data current-hypothesis iteration-fx curr-state)
  (let* ([new-hyp-score (induce-one-step background data current-hypothesis)]
         [shouldstop-state (iteration-fx current-hypothesis new-hyp-score curr-state)]
         [shouldstop (first shouldstop-state)]
         [next-state (second shouldstop-state)])
    (cond [shouldstop (list current-hypothesis next-state)]
          [else (feature-induction-gen background data (first new-hyp-score) iteration-fx next-state)])))

(define (induce-one-step background data current-hypothesis)
  (let* ([refinements (get-refinements background data current-hypothesis)]
         [refinement-scores (zip refinements
                                 (map (curry data-hyp->log-likelihood data) refinements))]
         [best-refinement-score (argmax second refinement-scores)])
    best-refinement-score))


(define (get-refinements background data current-hypothesis)
  ; We need generators of predicates and indexings
  ; We take them from the data itself.

  (define (generate-predicate-applications)
    (let* ([all-vars (iota (length (first data)))]
           [all-applications (lambda (b)
                               (cond [(commutative? b) (select-k-comm (pred->arity b) all-vars)]
                                     [else (select-k (pred->arity b) all-vars)]))])
      (concatenate (map (lambda (b)
                          (map (lambda (app)
                                 (list b app)) (all-applications b)))
                        background))))

  (define (already-used? pred-idx)
    (contains? pred-idx current-hypothesis))

  (define (legal-app? pred-idx)
    (and (not (already-used? pred-idx))
         (can-apply? (first pred-idx) 
                     (idx->vals (first data) (second pred-idx)))))

  (let* ([applications (generate-predicate-applications)]
         [legal-refinements (filter legal-app? applications)])
    (map (lambda (r) (cons r current-hypothesis)) legal-refinements)))

(define (idx->vals row idx)
  (map (lambda (i) (list-ref row i)) idx))

(define (data-hyp->log-likelihood data hyp)
  (define (single-log-likelihood row)
    (define (apply-one-pred pred-idx)
      (let* ([pred (first pred-idx)]
             [idx (second pred-idx)]
             [val (log (apply pred (idx->vals row idx)))])
        val))
    (apply + (map apply-one-pred hyp)))
  (apply + (map single-log-likelihood data)))

(define (argmax f xs)
  (cond [(null? xs) '()]
        [else 
          (first (sort (lambda (x y)
                         (> (f x) (f y))) xs))]))

