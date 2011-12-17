(import (printing)
        (grammar-induction)
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

(load example-data-file)

(define induced-grammar
  (gi-bmm data 1000 1.0 1.0 1.0 8 #t))

(define cleaned-seq
  (delete-duplicates-by-hash grammar-sort (grammar->history induced-grammar)))

(pretty-print cleaned-seq)
