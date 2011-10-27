(library (grammar-likelihood)
  (export
    batch-data-grammar->posterior)
  (import
    (rnrs)
    (program-likelihood)
    (chart-parsing)
    (program)
    (util))
  (define (grammar-size prog)
    (+ (apply + (map (lambda (abstr) (+ 1  ;; + 1: The "separator" symbol between nonterminals basically encourages merging
                                        (cond [(eq? 'choose (car (abstraction->pattern abstr))) ;; Choose operator does not count, so subtract 1 for using choose
                                               (- (sexpr-size (abstraction->pattern abstr)) 1)]
                                              [else (sexpr-size (abstraction->pattern abstr))])))
                     (program->abstractions prog)))
       (sexpr-size (program->body prog))))
  (define (grammar-prior prog)
    ;; (begin (print "in program->prior: grammar size: ~s" (grammar-size prog))
    (- (grammar-size prog)))
  (define (batch-data-grammar->posterior data progs . params)

    (define use-features? (= 3 (length params)))

           (define (iterator charts 
                             programs 
                             scores)
             (cond [(null? programs) (reverse scores)]

                   [else (let* ([likelihood-weight (cond [(null? params) 1.0]
                                                         [else (car params)])]
                                [prior-weight (cond [(null? params) 1.0]
                                                    [else (cadr params)])]
                                [prior (* prior-weight (grammar-prior (car programs)))]
                                ;; [db (begin (pretty-print (list likelihood-weight prior-weight prior)))]

                                )

                           (cond [(no-choices? (car programs)) (iterator charts 
                                                                         (cdr programs) 
                                                                         (cons prior scores))]
                                 [else 

                                   (let* ([inside-prob-fx (cond [use-features? parse-dag+features->log-prob]
                                                                [else exec-chart->log-prob])]
                                          [individual-likelihoods (map inside-prob-fx (car charts)) ]
                                          [likelihood 
                                            (* likelihood-weight
                                               (apply 
                                                 +
                                                 ;; + ;; product of the exemplar probabilities
                                                 ;; log-prob-sum2 ;; sum of exemplar probabilities
                                                 individual-likelihoods))]
                                          ;; [db (print "sum-likelihood: ~s" likelihood)]
                                          ;; [db (print "product-likelihood: ~s" (apply + individual-likelihoods))]
                                          ;; [db (print "individual likelihoods ~s" individual-likelihoods)]
                                          ;; [db (print "*****************sum-posterior ~s" (+ likelihood prior))]
                                          ;; [db (newline)]
                                          )

                                     (iterator (cdr charts) 
                                               (cdr programs) 
                                               (cons (+ likelihood 
                                                        prior) scores)))]))]))

           (let* ([progs-with-choices (filter (lambda (p) (not (no-choices? p))) progs)]
                  [all-charts (if (null? progs-with-choices) '() 
                                (cond [use-features? 
                                        (batch-run-inversion progs-with-choices data 'use-features)]
                                      [else 
                                        (batch-run-inversion progs-with-choices data)]
                                      ))]
                  [scores (iterator all-charts progs '())])
             (begin ;; (print "batch scores: ~s" scores)
               scores)))
  )
