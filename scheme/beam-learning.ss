(library (beam-learning)
         (export beam-search
                 learn-model)

         (import (rnrs)
                 (_srfi :1)
                 (chart-parsing)
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
             (begin (iter-fx fringe depth)
                    (cond [(null? fringe) "STARVED"]
                          [(= 0 depth) (car fringe)]
                          [else (beam-search fringe
                                             beam-size
                                             (- depth 1)
                                             pt->fringe
                                             pt->score
                                             fringe-merge
                                             iter-fx)]))))

         (define (learn-model data beam-size depth)
           (define (incorporate-data xs)
             (list 'lambda '() (cons 'choose xs)))

           (define (program->transforms prog)
             (begin
               (cons prog (compressions prog))))

           (define (program->log-posterior prog)
             (apply + (map (lambda (d) (data-program->log-posterior d prog))
                           data)))

           (let* ([initial-prog (make-program '() (incorporate-data data))]
                  [learned-program (beam-search (list initial-prog)
                                                beam-size depth
                                                program->transforms
                                                program->log-posterior
                                                (lambda (x) x)
                                                (lambda (fringe depth) (if (not (null? fringe)) 
                                                                   (begin (print "depth: ~s best program:" depth)
                                                                          (pretty-print (car fringe)))
                                                                   '())))])
             learned-program))



         )
                 
