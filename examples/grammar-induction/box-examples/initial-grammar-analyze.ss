(import (printing)
        (grammar-induction)
        (grammar-derivations-spread)
        (grammars)
        (bayes-program-merging)
        (scene-graphs)
        (program-likelihood)
        (util)
        (_srfi :1)
        (delimcc-simple-ikarus)
        (write-boxes)
        )


(define argv (command-line))

(define
  example-data-file
  (list-ref argv 1))

(define (get-merge-seq grammar)
  (delete-duplicates-by-hash grammar-sort (grammar->history grammar)))

(load example-data-file)

(define (get-new-models summ2 summ1)
  (let* ([models2 (map cadr summ2)]
         [models1 (map cadr summ1)]
         [m1-not-m2 (lset-difference equal? models1 models2)]
         [m2-not-m1 (lset-difference equal? models2 models1)])
    m2-not-m1))
  
(let*
  ([run-induction? (null? merge-history)]
   [merge-history (cond [run-induction? (get-merge-seq
                                          (gi-bmm data 1000 1.0 1.0 1.0 8 #t))]
                        [else merge-history])]
   [grammar-summaries (map (lambda (grammar) (list grammar (grammar-derivations grammar 0.01))) merge-history)]
   [model-summaries (map cadr grammar-summaries)]
   )

  (for-each pretty-print (map (lambda (idx)
                                `(new-models-for-grammar ,idx ,(+ 1 idx)
                                                         ,(get-new-models 
                                                            (list-ref model-summaries (+ 1 idx))
                                                            (list-ref model-summaries idx))))
                              (init (indices model-summaries))))
  ;;(for-each write-graffle (concatenate
                          ;;(map summary->graffles (indices grammar-summaries) grammar-summaries)))
  
  )


