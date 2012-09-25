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
                 (_srfi :1)
                 (delimcc-simple-ikarus))

         ;; bpm for the examples in the tech report
         (define (bpm data beam-size likelihood-weight prior-weight . stop-at-depth)
           ;; data: a tree with gaussian nodes
           ;;

           (define initial-population
             (map (lambda (e)
                    (subexpr-walk 
                      (lambda (t) (cond [(eq? 'gaussian (car t)) (sample-gaussian (cadr t) (caddr t))]
                                        [else t]))
                      e))
                  data))

           (define (valid? prog) 
             (no-higher-order-applications prog))

           (define (is-higher-order? abstr)
             (define pattern (abstraction->pattern abstr))
             (define (is-l-term? expr)
               (eq? 'lambda (car expr)))
             (define (l-term->vars e) (cadr e))

             (define (higher-order-app? e)
               (and (list? e) (var? (car e))))

             (define (remove-local-var-decls t)
               (cond [(is-l-term? t) `(lambda () ,(caddr t))]
                     [else t]))

             (define no-local-decls (subexpr-walk remove-local-var-decls pattern))

             (reset
               (begin
                 (map
                   (lambda (t) (cond [(higher-order-app? t) (begin
                                                              ;; (print "this is higher order:")
                                                              ;; (pretty-print t)
                                                              ;; (print "in expr:")
                                                              ;; (pretty-print no-local-decls)
                                                              (shift k #t))]
                                     [else t]))
                   (all-subexprs no-local-decls))
                 (shift k #f))))


             ;; (not (null? (deep-find-all
                           ;; higher-order-app?
                           ;; (subexpr-walk remove-local-var-decls pattern)))))

           (define (no-higher-order-applications prog)
             (let* ([abstrs (program->abstractions prog)]
                    [has-higher-order-app (filter is-higher-order? abstrs)])
               (null? has-higher-order-app)))


           (define (incorporate-data xs)
             (list 'lambda '() (cons 'choose xs)))

           (define (program->transforms prog)
             (let* ([transformed
                      (filter valid? 
                              (append 
                                       (compressions prog) 
                                       ;; (uniform-choose-dearguments prog)
                                       (recursive-choose-dearguments prog)
                                       ;; (same-variable-dearguments prog)
                                       ;; (arith-dearguments prog)
                                       )
                              )
                      ])
               (begin
                 (print "# candidates: ~s" (length transformed))
                 (pretty-print `(rec-ch ,(recursive-choose-dearguments prog)
                                       valid ,(filter valid? (recursive-choose-dearguments prog))))
                 ;; (pretty-print transformed)
                 transformed)))

           (define (print-stats fringe depth)
             (let ([best-prog (caar fringe)])
               (begin (print "depth: ~s best programs:" depth)
                      (print "posterior: ~s" (cdar fringe))
                      (pretty-print (max-take fringe 5))
                      ;; (pretty-print (cdr fringe))
                      )))

           (define (depth-stop fringe depth)
             (begin (print-stats fringe depth)
                    (= 0 depth)))

           (define (same-prog-stop limit)
             (define prog-store '())

             (define (add-one-prog-likelihood p) 
               (let ([program (car p)]
                     [likelihood (cadr p)])
                 (set! prog-store (cons (list program likelihood) prog-store))))

             (define (reached-limit?)
               (let ([tail (max-take prog-store limit)])
                 (cond [(>= (length tail) limit) (= 1 (length (delete-duplicates tail)))]
                       [else #f])))

             (lambda (fringe depth)
               (begin (print-stats fringe depth)
                      (add-one-prog-likelihood (car fringe))
                      (reached-limit?))))


           (define (score-programs progs) 
             (batch-data-program->posterior data progs likelihood-weight prior-weight))

           (let* ([db (pretty-print data)]
                  [initial-prog (make-program '() (incorporate-data data))]
                  [db (pretty-print initial-prog)]
                  [learned-program (beam-search3
                                     (zip (list initial-prog) (score-programs (list initial-prog)))
                                     (list initial-prog (car (score-programs (list initial-prog))))
                                     beam-size
                                     program->transforms
                                     score-programs
                                     (lambda (fringe) fringe)
                                     (if (not (null? stop-at-depth)) depth-stop (same-prog-stop 50)))]
                                     )
             learned-program))
         )
