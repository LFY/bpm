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

(define (repeat-apply f n x)
  (cond [(= 0 n) x]
        [else (repeat-apply f (- n 1) (f x))]))

(define (is-finished-model? tree)
  (reset
    (begin
      (tree-walk
        (lambda (t) (cond [(procedure? t) (shift k #f)]
                          [else t]))
        tree)
      #t)))


(define num-models 10)
(define (postprocess-distr curr)
  (max-take
    (sort (lambda (x y) (> (car x) (car y)))
        curr)
    num-models))

(define (list-models eps curr to-expand)
  (let* ([next (explore to-expand)]
         [keep (filter (lambda (next-elt)
                         (let* ([prob (car next-elt)]
                                [elt (cadr next-elt)])
                           (cond 
                             [(> prob eps) #t]
                             [else #f])))
                       next)])
    (cond [(null? (filter (lambda (x) (not (is-finished-model? x))) keep)) 
           (postprocess-distr curr)]
          [else (begin
                  (list-models
                    eps
                    (append curr (filter is-finished-model? keep))
                    next))])))

(define (generic-list-models curr to-expand stop? postprocess)
  (let* ([next (explore to-expand)]
         [next-res (postprocess (append curr next))])
    (cond [(or (equal? to-expand next) 
               (stop? next next-res)) next-res]
          [else 
            (generic-list-models next-res next stop? postprocess)])))

(define (top-n score-fx n xs)
  (max-take
    (sort
      (lambda (x y)
        (> (score-fx x)
           (score-fx y)))
      xs)
    n))

(define node->prob car)

(define eps 0.1)

(define (my-postprocess current)
  (filter (lambda (n) (> (node->prob n) eps)) (filter is-finished-model? current)))


(define (my-stop? next current)
  (null? (filter (lambda (n) (> (node->prob n) eps)) next)))

(load example-data-file)

(define (grammar->character eps grammar)
  (grammar-derivations grammar eps))

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


