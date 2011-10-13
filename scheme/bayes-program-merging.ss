(library (bayes-program-merging)
         (export bpm)
         (import (rnrs)
                 (beam-learning)
                 (printing)
                 (util)
                 (program-likelihood)
                 (abstract)
                 (program)
                 (dearguments)
                 (_srfi :1))

         ;; bpm for the examples in the tech report
         (define (bpm data beam-size depth)
           ;; data: a tree with gaussian nodes
           ;;

           (define initial-population
             (map (lambda (e)
                    (subexpr-walk 
                      (lambda (t) (cond [(eq? 'gaussian (car t)) (sample-gaussian (cadr t) (caddr t))]
                                        [else t]))
                      e))
                  data))

           (define (incorporate-data xs)
             (list 'lambda '() (cons 'choose xs)))

           (define (program->transforms prog)
             (begin
               (cons prog (append (compressions prog) 
                                  (uniform-choose-dearguments prog)
                                  (recursive-choose-dearguments prog)
                                  (same-variable-dearguments prog)
                                  (noisy-number-dearguments prog)
                                  ))))

           (define (program->log-posterior prog)
             (apply + (map (lambda (d) (data-program->log-posterior2 d prog))
                           initial-population)))
           (define (print-stats fringe depth)
             (let ([best-prog (car fringe)])
               (begin (print "depth: ~s best program:" depth)
                      (print "posterior: ~s" (program->log-posterior best-prog))
                      (pretty-print (car fringe)))))


           (define (depth-stop fringe depth)
             (begin (print-stats fringe depth)
                    (= 0 depth)))

           (let* ([db (pretty-print initial-population)]
                  [initial-prog (make-program '() (incorporate-data data))]
                  [db (pretty-print initial-prog)]
                  [learned-program (beam-search-batch-score (list initial-prog)
                                                            beam-size depth
                                                            program->transforms
                                                            (lambda (progs) (batch-data-program->posterior initial-population progs 1.0 1.0 'use-features))
                                                            (lambda (x) x)
                                                            depth-stop)])
             learned-program))
         )
