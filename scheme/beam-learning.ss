(library (beam-learning)
         (export beam-search
                 learn-model)

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

         (define (learn-model data beam-size depth)
           (define (incorporate-data xs)
             (list 'lambda '() (cons 'choose xs)))

           (define (program->transforms prog)
             (begin
               (cons prog (append (compressions prog) 
                                  (uniform-choose-dearguments prog)
                                  (recursive-choose-dearguments prog)
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
                  [learned-program (beam-search (list initial-prog)
                                                beam-size depth
                                                program->transforms
                                                program->log-posterior
                                                (lambda (x) x)
                                                (same-prog-stop 5))])
             learned-program))



         )

