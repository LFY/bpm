(import (printing)
        (grammar-induction)
        (analyze-grammar)
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

(let*
  ([run-induction? (null? merge-history)]
   [merge-history (cond [run-induction? (get-merge-seq
                                          (gi-bmm data 1000 1.0 1.0 1.0 8 #t))]
                        [else merge-history])])
  (begin
    (print "Productions with probability > 0.01, for each grammar in the sequence:")
    (pretty-print (map (lambda (grammar)
                         (begin
                           (print "# Grammar + top derivations:")
                           (pretty-print grammar)
                           (pretty-print (list-models 0.001 '() (get-initial-distr grammar)))
                           ))
                       merge-history))))



