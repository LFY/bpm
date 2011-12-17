(import (printing)
        (grammar-induction)
        (grammar-derivations-spread)
        (grammars)
        (bayes-program-merging)
        (scene-graphs)
        (program-likelihood)
        (util)
        (_srfi :1)
        )


(define argv (command-line))

(define
  example-data-file
  (list-ref argv 1))

(define (get-merge-seq grammar)
  (delete-duplicates-by-hash grammar-sort (grammar->history grammar)))

(load example-data-file)

(let*
  ([run-induction? (null? merge-history)]
   [merge-history (cond [run-induction? (get-merge-seq
                                          (gi-bmm data 1000 1.0 1.0 1.0 8 #t))]
                        [else merge-history])])
  (begin
    (print "Productions with probability > 0.001, for each grammar in the sequence:")
    (pretty-print (map (lambda (grammar)
                         (begin
                           (print "# Grammar + top derivations:")
                           (pretty-print grammar)
                           (pretty-print (grammar-derivations grammar 0.001))))
                       merge-history))))




