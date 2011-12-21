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
(define prefix example-data-file)

(define (summary->graffles summary-idx grammar-summary)
  (let* ([cleaned-summary (delete-duplicates (cadr grammar-summary))]
         [graffles 
           (map (lambda (model-idx prob-model)
                  (let* (
                         [prob (car prob-model)]
                         [graffle-name (string-append 
                                         prefix
                                         "_" 
                                         (number->string summary-idx) 
                                         "_" 
                                         (number->string model-idx)
                                         "_"
                                         (number->string prob) 
                                         ".sxml")]
                         [scene (cadr prob-model)]
                         [graffle-scene (box-scene->graffle scene)]
                         )
                    `(graffle 
                       ,graffle-name
                       ,graffle-scene)))
                (indices cleaned-summary) cleaned-summary)])
    graffles))

(define (write-graffle graffle)
  (let* ([filename (cadr graffle)]
         [sxml (caddr graffle)])
    (begin
      (system (format "rm ~s" filename))
      (with-output-to-file filename
                           (lambda ()
                             (pretty-print sxml)))
      (system (string-append "python reconst-graffle.py " filename " " (string-append filename ".graffle"))))))

(let*
  ([run-induction? (null? merge-history)]
   [merge-history (cond [run-induction? (get-merge-seq
                                          (gi-bmm data 1000 1.0 1.0 1.0 8 #t))]
                        [else merge-history])]
   [grammar-summaries (map (lambda (grammar) (list grammar (grammar-derivations grammar 0.004))) merge-history)]
   [model-summaries (map cadr grammar-summaries)]
   )

  (for-each write-graffle (concatenate
                            (map summary->graffles (indices grammar-summaries) grammar-summaries)))

  )


