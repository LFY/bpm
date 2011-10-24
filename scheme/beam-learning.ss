(library (beam-learning)
         (export beam-search
                 beam-search2
                 beam-search3
                 learn-model
                 beam-search-batch-score)

         (import (rnrs)
                 (_srfi :1)
                 (named-search-trees)

                 (program)
                 (abstract)
                 (dearguments)
                 (sym)

                 (program-likelihood)

                 (util)

                 (printing))


         (define (beam-search current-pts 
                              beam-size 
                              depth 
                              pt->fringe 
                              pt->score 
                              fringe-merge
                              iter-fx)
           (let* ([fringe (max-take (sort-by pt->score >
                                             (fringe-merge 
                                               (concatenate 
                                                 (map pt->fringe current-pts))))
                                    beam-size)])
             (cond [(null? fringe) "STARVED"]
                   [(iter-fx fringe depth) (car fringe)]
                   [else (beam-search fringe
                                      beam-size
                                      (- depth 1)
                                      pt->fringe
                                      pt->score
                                      fringe-merge
                                      iter-fx)])))

         (define (beam-search-batch-score current-pts 
                                          beam-size 
                                          depth 
                                          pt->fringe 
                                          fringe->score
                                          fringe-merge
                                          iter-fx)
           (let* ([fringe (map car (let* ([results 
                                            (max-take 
                                              (sort-by second >
                                                       (let* ([fringe0 (concatenate (map pt->fringe current-pts))])
                                                         (fringe-merge (zip fringe0 (fringe->score fringe0)))))
                                              beam-size)])
                                     (begin ;; (pretty-print results)
                                       results)))])
             (cond [(null? fringe) (begin (print "STARVED")
                                          (car current-pts))]
                   [(iter-fx fringe depth) (car fringe)]
                   [else (beam-search-batch-score fringe
                                                  beam-size
                                                  (- depth 1)
                                                  pt->fringe
                                                  fringe->score
                                                  fringe-merge
                                                  iter-fx)])))

         (define (max-tail xs n)
           (cond [(null? xs) xs]
                 [(= 0 n) xs]
                 [else (max-tail (cdr xs) (- n 1))]))

         (define (beam-search2 fringe
                               beam-size
                               depth
                               pt->fringe
                               fringe->score
                               fringe-merge
                               iter-fx)

           (let* (
                  [to-expand (max-take fringe beam-size)]
                  [remaining-unexpanded (max-tail fringe beam-size)]
                  [db (print (map cadr to-expand))]
                  [expanded-pts (concatenate (map pt->fringe (map car to-expand)))]
                  [expanded-pt-scores (zip expanded-pts (fringe->score expanded-pts))]
                  [new-fringe (sort-by second > (fringe-merge (append remaining-unexpanded expanded-pt-scores)))]
                  )
             (cond [(null? new-fringe) (caar fringe)]
                   [(iter-fx new-fringe depth) (caar new-fringe)]
                   [else 
                     (beam-search2 new-fringe
                                   beam-size
                                   (- depth 1)
                                   pt->fringe
                                   fringe->score
                                   fringe-merge
                                   iter-fx)])))

         (define (cmp-pt pt1 pt2)
           (let* ([score1 (cadr pt1)]
                  [score2 (cadr pt2)])
             (cond [(> score1 score2) 'GT]
                   [(< score1 score2) 'LT]
                   [(= score1 score2) 'EQ])))

         (define (beam-search3 unexpanded
                               best-pt-score
                               beam-size
                               pt->fringe
                               fringe->score
                               fringe-merge
                               iter-fx)
           (let* ([db (print "# nodes in fringe: ~s" (length unexpanded))]
                  [to-expand (caar unexpanded)] ;; Pick the highest scoring point
                  [expanded-pts (pt->fringe to-expand)] ;; Expand it

                  ;; Calculate scores, merge identical grammars and sort fringe. Ideally, we would use an ordered hash-table.
                  [expanded-pt-scores (sort-by second > (fringe-merge (zip expanded-pts (fringe->score expanded-pts))))]

                  ;; Get the best-scoring <beam-size> points 
                  [best-scoring (max-take expanded-pt-scores beam-size)]

                  ;; Add them to the unexpanded list and merge. (the new fringe)
                  [new-unexpanded (sort-by second > (fringe-merge (append (cdr unexpanded) best-scoring)))]

                  ;; Track the best point.
                  [new-best-pt-score (if (and (not (null? new-unexpanded))
                                              (or (eq? 'GT (cmp-pt (car new-unexpanded) best-pt-score))
                                                  (eq? 'EQ (cmp-pt (car new-unexpanded) best-pt-score)))
                                                       )
                                       (car new-unexpanded)
                                       best-pt-score)]
                  
                  )
             (cond [(null? new-unexpanded) (car best-pt-score)]
                   [(iter-fx (cons new-best-pt-score new-unexpanded) 0) (car new-best-pt-score)]
                   ;; [(iter-fx new-unexpanded 0) (car best-pt-score)]
                   [else 
                     (beam-search3 new-unexpanded
                                   new-best-pt-score
                                   beam-size
                                   pt->fringe
                                   fringe->score
                                   fringe-merge
                                   iter-fx)])))



         (define (learn-model data beam-size depth)
           (define (incorporate-data xs)
             (list 'lambda '() (cons 'choose xs)))

           (define (program->transforms prog)
             (begin
               (cons prog (append (compressions prog) 
                                  ;; (uniform-choice-compressions prog)
                                  (uniform-choose-dearguments prog)
                                  (recursive-choose-dearguments prog)
                                  ;; (arith-dearguments prog)
                                  (same-variable-dearguments prog)
                                  (noisy-number-dearguments prog)
                                  ))))

           (define (program->log-posterior prog)
             (apply + (map (lambda (d) (data-program->log-posterior d prog))
                           data)))
           (define (print-stats fringe depth)
             (let ([best-prog (car fringe)])
               (begin (print "depth: ~s best program:" depth)
                      (print "posterior: ~s" (program->log-posterior best-prog))
                      (pretty-print (car fringe)))))


           (define (depth-stop fringe depth)
             (= 0 depth))

           (define (same-prog-stop limit)
             (define prog-store '())
             (define (add-one-prog p) (set! prog-store (cons p prog-store)))
             (define (reached-limit?)
               (let ([tail (max-take prog-store limit)])
                 (cond [(>= (length tail) limit) (= 1 (length (delete-duplicates tail)))]
                       [else #f])))

             (lambda (fringe depth)
               (begin (print-stats fringe depth)
                      (add-one-prog (car fringe))
                      (reached-limit?))))

           (let* ([initial-prog (make-program '() (incorporate-data data))]
                  [db (pretty-print initial-prog)]
                  [learned-program (beam-search-batch-score (list initial-prog)
                                                            beam-size depth
                                                            program->transforms
                                                            (lambda (progs) (batch-data-program->posterior data progs))
                                                            (lambda (x) x)
                                                            (same-prog-stop 10))])
             learned-program))



         )

