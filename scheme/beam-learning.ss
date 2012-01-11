(library (beam-learning)
         (export 
           beam-search-unlimited-fringe
           beam-search-const-mem
           beam-search-local
           beam-search-full
           beam-search-stochastic
           beam-search-stochastic-single
           
           rnd-select2)

         (import (except (rnrs) string-hash string-ci-hash)
                 (_srfi :1)
                 (_srfi :69)
                 (named-search-trees)

                 (program)
                 (abstract)
                 (forkmap)
                 (dearguments)
                 (sym)

                 (program-likelihood)

                 (util)

                 (printing))




         (define (max-tail xs n)
           (cond [(null? xs) xs]
                 [(= 0 n) xs]
                 [else (max-tail (cdr xs) (- n 1))]))


         (define (cmp-pt pt1 pt2)
           (let* ([score1 (cadr pt1)]
                  [score2 (cadr pt2)])
             (cond [(> score1 score2) 'GT]
                   [(< score1 score2) 'LT]
                   [(= score1 score2) 'EQ])))

         (define (beam-search-unlimited-fringe
                   unexpanded
                   best-pt-score
                   beam-size
                   pt->fringe
                   pre-filter-fringe
                   update+score-fringe
                   fringe-merge
                   iter-fx)
           (let* (
                  [db (print "# nodes in fringe: ~s" (length unexpanded))]
                  [to-expand (caar unexpanded)]
                  [expanded-pts (pre-filter-fringe (pt->fringe to-expand))] 
                  [updated-fringe+scores (update+score-fringe expanded-pts)]
                  [merged-updated-fringe+scores (fringe-merge updated-fringe+scores)]
                  [expanded-pt-scores (sort-by second > merged-updated-fringe+scores)]
                  [best-scoring (max-take expanded-pt-scores beam-size)]
                  [db (begin
                        (print "beam search stats")
                        (print "length of expanded-pts ~s" (length expanded-pts))
                        (print "length of merged-updated-fringe+scores ~s" (length merged-updated-fringe+scores))
                        (print "length of expanded-pt-scores ~s" (length expanded-pt-scores))
                        (print "length of best-scoring ~s" (length best-scoring))

                        (print "top scores:")
                        (pretty-print (map cadr best-scoring)))]
                  [new-unexpanded (sort-by second > (fringe-merge (append (cdr unexpanded) best-scoring)))] ;; a possibility: we cycle between a bunch of grammars here even though we might just have 1 of each grammar at each likelihood
                  [db (begin
                        (print "top 10 new-unexpanded scores:")
                        (pretty-print (map cadr (max-take new-unexpanded 10))))]
                  [new-best-pt-score (if (and (not (null? new-unexpanded))
                                              ;; new: don't replace our 'best grammar' with another unless it is _strictly_ better
                                              ;;(or (eq? 'GT (cmp-pt (car new-unexpanded) best-pt-score))
                                              ;;(eq? 'EQ (cmp-pt (car new-unexpanded) best-pt-score)))
                                              (eq? 'GT (cmp-pt (car new-unexpanded) best-pt-score))
                                              )
                                       (car new-unexpanded)
                                       best-pt-score)]

                  )
             (cond [(null? new-unexpanded) (car best-pt-score)]
                   [(iter-fx (cons new-best-pt-score new-unexpanded) 0) (car new-best-pt-score)]
                   ;; [(iter-fx new-unexpanded 0) (car best-pt-score)]
                   [else 
                     (beam-search-unlimited-fringe new-unexpanded
                                                   new-best-pt-score
                                                   beam-size
                                                   pt->fringe
                                                   pre-filter-fringe
                                                   update+score-fringe
                                                   fringe-merge
                                                   iter-fx)])))

         (define (beam-search-const-mem
                   unexpanded
                   best-pt-score
                   beam-size
                   pt->fringe
                   pre-filter-fringe
                   update+score-fringe
                   fringe-merge
                   iter-fx)
           (let* (
                  [db (print "# nodes in fringe: ~s" (length unexpanded))]
                  [to-expand (caar unexpanded)]
                  [expanded-pts (pre-filter-fringe (pt->fringe to-expand))] 
                  [updated-fringe+scores (update+score-fringe expanded-pts)]

                  ;; do NOT cut it off here.
                  ;;[best-scoring (max-take expanded-pt-scores beam-size)]

                  ;; cut off HERE
                  [new-unexpanded (max-take (sort-by second > (fringe-merge (append (cdr unexpanded) updated-fringe+scores))) beam-size)] 
                  [db (begin
                        (print "curr beam size ~s" (length new-unexpanded))
                        (print "top 10 new-unexpanded scores:")
                        (pretty-print (map cadr (max-take new-unexpanded 10))))]
                  [new-best-pt-score (if (and (not (null? new-unexpanded))
                                              ;; new: don't replace our 'best grammar' with another unless it is _strictly_ better
                                              ;;(or (eq? 'GT (cmp-pt (car new-unexpanded) best-pt-score))
                                              ;;(eq? 'EQ (cmp-pt (car new-unexpanded) best-pt-score)))
                                              (eq? 'GT (cmp-pt (car new-unexpanded) best-pt-score))
                                              )
                                       (car new-unexpanded)
                                       best-pt-score)]

                  )
             (cond [(null? new-unexpanded) (car best-pt-score)]
                   [(iter-fx (cons new-best-pt-score new-unexpanded) 0) (car new-best-pt-score)]
                   ;; [(iter-fx new-unexpanded 0) (car best-pt-score)]
                   [else 
                     (beam-search-const-mem new-unexpanded
                                            new-best-pt-score
                                            beam-size
                                            pt->fringe
                                            pre-filter-fringe
                                            update+score-fringe
                                            fringe-merge
                                            iter-fx)])))
(define (beam-search-full
          unexpanded best-pt-score beam-size
          pt->fringe pre-filter-fringe update+score-fringe
          fringe-merge iter-fx)
  (define database (make-hash-table equal?))
  (define (beam-search-full-loop
            unexpanded
            best-pt-score
            beam-size
            pt->fringe
            pre-filter-fringe
            update+score-fringe
            fringe-merge
            iter-fx)
    (let* (
           [db (print "# nodes in fringe: ~s" (length unexpanded))]
           [void (hash-table-set! database (car unexpanded) 'AlreadySeen)]
           [to-expand (caar unexpanded)]
           [expanded-pts (pre-filter-fringe (pt->fringe to-expand))] 
           [updated-fringe+scores (update+score-fringe expanded-pts)]
           [new-unexpanded (filter (lambda (pt) (not (hash-table-exists? database pt)))
                                   (sort-by second > (fringe-merge (append (cdr unexpanded) updated-fringe+scores))))] 
           [db (print "change in fringe: ~s" (- (length new-unexpanded) (length unexpanded)))]

           [db (begin
                 (print "(really breadth first search) curr beam size ~s" (length new-unexpanded))
                 (print "top 10 new-unexpanded scores:")
                 (pretty-print (map cadr (max-take new-unexpanded 10))))]
           [new-best-pt-score (if (and (not (null? new-unexpanded))
                                       ;; new: don't replace our 'best grammar' with another unless it is _strictly_ better
                                       ;;(or (eq? 'GT (cmp-pt (car new-unexpanded) best-pt-score))
                                       ;;(eq? 'EQ (cmp-pt (car new-unexpanded) best-pt-score)))
                                       (eq? 'GT (cmp-pt (car new-unexpanded) best-pt-score))
                                       )
                                (car new-unexpanded)
                                best-pt-score)]
           [db (iter-fx (cons new-best-pt-score new-unexpanded) 0)]
           )
      (cond [(null? new-unexpanded) (car best-pt-score)]
            [else 
              (beam-search-full-loop new-unexpanded
                                     new-best-pt-score
                                     beam-size
                                     pt->fringe
                                     pre-filter-fringe
                                     update+score-fringe
                                     fringe-merge
                                     iter-fx)])))
  (beam-search-full-loop unexpanded best-pt-score beam-size
                         pt->fringe pre-filter-fringe update+score-fringe
                         fringe-merge iter-fx))

(define (log-rnd-select2 log-vps)
  (let* ([pvec (map cadr log-vps)]
         [norm-const (apply log-prob-sum2 pvec)]
         [res (map exp (map (lambda (p) (- p norm-const)) pvec))]
         )
    (car (rnd-select2 (zip log-vps res)))))

(define (rnd-select2 vps)
           (cond [(null? vps) '()]
                 [else 
                   (letrec* ([smp (uniform-sample 0 1)]
                             [vps* (zip vps (scan1 + (map cadr vps)))]
                             [iterator (lambda (vps)
                                         (let* (
                                                [vp (car vps)]
                                                [p (cadr vp)]
                                                [v (car vp)])
                                           (cond [(< smp p) v]
                                                 [(null? (cdr vps)) v]
                                                 [else (iterator (cdr vps))])))])
                            (iterator vps*))]))
(define (beam-search-stochastic
          unexpanded
          best-pt-score
          beam-size
          pt->fringe
          pre-filter-fringe
          update+score-fringe
          fringe-merge
          iter-fx)
  (define (loop
            unexpanded
            best-pt-score
            beam-size
            pt->fringe
            pre-filter-fringe
            update+score-fringe
            fringe-merge
            iter-fx)
    (let* (
           [db (print "# nodes in fringe: ~s" (length unexpanded))]
           [expanded-pts (pre-filter-fringe 
                           (concatenate 
                             (forkmap 
                               (lambda (pt) (pre-filter-fringe (pt->fringe pt))) 
                               (map car unexpanded) 8)))]
           [db (print "expanded")]
           [updated-fringe+scores (update+score-fringe expanded-pts)]
           [db (print "scored")]
           [new-fringe (fringe-merge updated-fringe+scores)]
           [db (print "new fringe size: ~s" (length new-fringe))])
      (cond [(null? new-fringe) (car best-pt-score)]
            [else (let* ([new-unexpanded (fringe-merge (map (lambda (i) (log-rnd-select2 new-fringe)) (iota beam-size)))]
                         [db (begin
                               (print "stochastic-beam-search curr beam size ~s" (length new-unexpanded))
                               (print "top 10 new-unexpanded scores:")
                               (pretty-print (map cadr (max-take new-unexpanded 10))))]
                         [new-best-pt-score (if (and (not (null? new-unexpanded))
                                                     (eq? 'GT (cmp-pt (car new-unexpanded) best-pt-score))
                                                     )
                                              (car new-unexpanded)
                                              best-pt-score)])

                    (cond [(null? new-unexpanded) (car best-pt-score)]
                          [(iter-fx (cons new-best-pt-score new-unexpanded) 0) (car new-best-pt-score)]
                          ;; [(iter-fx new-unexpanded 0) (car best-pt-score)]
                          [else 
                            (loop new-unexpanded
                                  new-best-pt-score
                                  beam-size
                                  pt->fringe
                                  pre-filter-fringe
                                  update+score-fringe
                                  fringe-merge
                                  iter-fx)]))])))
  (loop unexpanded best-pt-score beam-size pt->fringe pre-filter-fringe update+score-fringe fringe-merge iter-fx))

(define (beam-search-stochastic-single
          unexpanded
          best-pt-score
          beam-size
          pt->fringe
          pre-filter-fringe
          update+score-fringe
          fringe-merge
          iter-fx)
  (define (loop
            unexpanded
            best-pt-score
            beam-size
            pt->fringe
            pre-filter-fringe
            update+score-fringe
            fringe-merge
            iter-fx)
    (let* (
           [db (print "# nodes in fringe: ~s" (length unexpanded))]
           [expanded-pts (pre-filter-fringe (pt->fringe (car (log-rnd-select2 unexpanded))))]
           [db (print "expanded")]
           [updated-fringe+scores (update+score-fringe expanded-pts)]
           [db (print "scored")]
           [new-fringe (fringe-merge updated-fringe+scores)]
           [db (print "new fringe size: ~s" (length new-fringe))])
      (cond [(null? new-fringe) (car best-pt-score)]
            [else (let* ([new-unexpanded (fringe-merge (map (lambda (i) (log-rnd-select2 new-fringe)) (iota beam-size)))]
                         [db (begin
                               (print "stochastic-beam-search-single curr beam size ~s" (length new-unexpanded))
                               (print "top 10 new-unexpanded scores:")
                               (pretty-print (map cadr (max-take new-unexpanded 10))))]
                         [new-best-pt-score (if (and (not (null? new-unexpanded))
                                                     (eq? 'GT (cmp-pt (car new-unexpanded) best-pt-score))
                                                     )
                                              (car new-unexpanded)
                                              best-pt-score)])

                    (cond [(null? new-unexpanded) (car best-pt-score)]
                          [(iter-fx (cons new-best-pt-score new-unexpanded) 0) (car new-best-pt-score)]
                          ;; [(iter-fx new-unexpanded 0) (car best-pt-score)]
                          [else 
                            (loop new-unexpanded
                                  new-best-pt-score
                                  beam-size
                                  pt->fringe
                                  pre-filter-fringe
                                  update+score-fringe
                                  fringe-merge
                                  iter-fx)]))])))
  (loop unexpanded best-pt-score beam-size pt->fringe pre-filter-fringe update+score-fringe fringe-merge iter-fx))

(define (beam-search-local
          unexpanded
          best-pt-score
          beam-size
          pt->fringe
          pre-filter-fringe
          update+score-fringe
          fringe-merge
          iter-fx)
  (let* (
         [db (print "# nodes in fringe: ~s" (length unexpanded))]
         [expanded-pts (pre-filter-fringe (concatenate (forkmap (lambda (pt) (pre-filter-fringe (pt->fringe pt))) 
                                                            (map car unexpanded)
                                                            8)))]
         [db (print "expanded")]
         [updated-fringe+scores (update+score-fringe expanded-pts)]
         [db (print "scored")]
         [new-unexpanded (max-take (sort-by second > (fringe-merge updated-fringe+scores)) beam-size)]
         [db (begin
               (print "local-beam-search: curr beam size ~s" (length new-unexpanded))
               (print "top 10 new-unexpanded scores:")
               (pretty-print (map cadr (max-take new-unexpanded 10))))]
         [new-best-pt-score (if (and (not (null? new-unexpanded))
                                     (eq? 'GT (cmp-pt (car new-unexpanded) best-pt-score))
                                     )
                              (car new-unexpanded)
                              best-pt-score)]

         )
    (cond [(null? new-unexpanded) (car best-pt-score)]
          [(iter-fx (cons new-best-pt-score new-unexpanded) 0) (car new-best-pt-score)]
          ;; [(iter-fx new-unexpanded 0) (car best-pt-score)]
          [else 
            (beam-search-local new-unexpanded
                                   new-best-pt-score
                                   beam-size
                                   pt->fringe
                                   pre-filter-fringe
                                   update+score-fringe
                                   fringe-merge
                                   iter-fx)])))

         )
