(library (concept-testing)
         (export run-multiple-data
                 replacement-fx
                 format-named-hypothesis)
         (import (rnrs)
                 (prob-logic)
                 (printing)
                 (program)
                 (dearguments)
                 (abstract)
                 (background-predicates)
                 (util)

                 (_srfi :1))

         (define (run-multiple-data data-names)
           (define (build-table next-col acc)
             (append acc next-col))
           (define (run-one-data data-name)
             (let* ([data (first data-name)]
                    [name (second data-name)]
                    [hyp-scores (feature-induction-n-iter 10 simple-soft-predicates data '() '())]
                    [learned-hyp (first hyp-scores)]
                    [first-col (first (second hyp-scores))]
                    [named-first-col (cons name first-col)]
                    [named-hyp (cons name (list learned-hyp))])
               (list named-hyp (list named-first-col))))
           (let* ([named-hyp-data-cols (map run-one-data data-names)]
                  [named-hyps (map first named-hyp-data-cols)]
                  [data-cols (map second named-hyp-data-cols)]
                  [max-len (apply max (map (lambda (x) (length (first x))) data-cols))]
                  [iter-col (list (cons 'Iteration (map (curry + 1) (iota max-len))))]
                  [all-cols (cons iter-col data-cols)])
             (list named-hyps (fold build-table '() all-cols))))

         (define replacement-fx (lambda (v) (cond [(or (eq? 'NULL v)
                                                       (eqv? -inf.0 v)) ""]
                                                  [else (format "~s" v)])))

         (define (format-named-hypothesis name-hyp)
           (let* ([name (first name-hyp)]
                  [hyp (second name-hyp)])
             (string-append "Name: " (str name) "\n" (format-hypothesis hyp)))) 
         )
