(library (grammar-likelihood)
  (export
    batch-data-grammar->posterior)
  (import
    (rnrs)
    (program-likelihood)
    (chart-parsing)
    (program)
    (parameter-estimation)
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

  (define (add-params params grammar)
    `(program
       ,(program->abstractions grammar)
       ,(program->body grammar)
       ,params))

  (define (postprocess-params grammar+params)
    '())

  (define (batch-data-grammar->posterior data progs . params)

           (define (iterator charts 
                             grammars 
                             parameterized-grammar+scores)
             (cond [(null? grammars) (reverse parameterized-grammar+scores)]

                   [else (let* ([likelihood-weight (cond [(null? params) 1.0]
                                                         [else (car params)])]
                                [prior-weight (cond [(null? params) 1.0]
                                                    [else (cadr params)])]
                                [prior (* prior-weight (grammar-prior (car grammars)))]
                                ;; [db (begin (pretty-print (list likelihood-weight prior-weight prior)))]

                                )

                           (cond [(no-choices? (car grammars)) (iterator charts 
                                                                         (cdr grammars) 
                                                                         (cons (list (car grammars) prior) parameterized-grammar+scores))]
                                 [else 

                                   (let* (
                                          [likelihood-parameters (train-parameters (map reformat-exec-chart (car charts)))]
                                          [likelihood (car likelihood-parameters)]
                                          [params (cadr likelihood-parameters)]
                                          [grammar+parameters (add-params params (car grammars))]
                                          )

                                     (iterator (cdr charts) 
                                               (cdr grammars) 
                                               (cons (list 
                                                       grammar+parameters 
                                                       (+ likelihood prior)) parameterized-grammar+scores)))]))]))

           (let* ([progs-with-choices (filter (lambda (p) (not (no-choices? p))) progs)]
                  [all-charts (if (null? progs-with-choices) '() 
                                (batch-run-inversion progs-with-choices data)
                                )]
                  [parameterized-grammar+scores (iterator all-charts progs '())])
             (begin ;; (print "batch scores: ~s" scores)
               parameterized-grammar+scores)))
  )
