(library (grammar-likelihood)
  (export
    batch-data-grammar->posterior
    grammar-prior
    single-data-grammar->likelihood
    grammar-size
    log-beta-function
    assign-uniform-params
    populate-stats
    )
  (import
    (except (rnrs) string-hash string-ci-hash)
    (_srfi :1)
    (_srfi :69)
    (program-likelihood)
    (chart-parsing)
    (program)
    (grammars)
    (parameter-estimation)
    (printing)
    (forkmap)
    (util))

  (define (grammar-size gr)
    (define (count-one-subcomponent s) 2) ;; (tr "f" (F1)) 2 symbols
    (define (count-one-choice c)
      (if (equal? 'elem (car c))
      (cond [(null? (cddr c)) 1] ;; just (elem "e")
            [else 
              (+ 1 (apply + (map count-one-subcomponent (cddr c))))]) ;; e.g., (elem "e" (tr "f" (F3))) 3 symbols
      1))
    (define (count-one-nt nt)
      (let* ([choices (nt->choices nt)])
        (+ 1 (apply + (map count-one-choice choices))))) ;; the 1 is for the separator
    (let* ([nts (grammar->nts gr)])
      (apply + (map count-one-nt nts))))

  (define (log-prob-normalize probs)
    (let* ([denom (log (apply + (map exp probs)))])
    (map (lambda (n) (- n denom)) probs)))

  (define (description-length-prior prog)
    (- (grammar-size prog)))

(define (dirichlet-prior prior-parameter parameters)
           (let* (
                  [prod-param (* (- prior-parameter 1) (apply + (log-prob-normalize parameters)))]
                  [prob-param (- prod-param (log-beta-function prior-parameter (length parameters)))])
             prob-param
             ))


(define (grammar-dirichlet-prior prior-parameter prog)
    (let* (
           [prior-param (apply + (map (lambda (params) (dirichlet-prior prior-parameter params))
                                      (filter (lambda (param-collection) (> (length param-collection) 1)) (grammar->params prog))))])
      prior-param
      ))

(define (grammar-prior prior-parameter prog)
    (let* ( [prior-struct (- (grammar-size prog))]
           [prior-param (grammar-dirichlet-prior prior-parameter prog)])
      (+ prior-struct 
         prior-param
         )))


  (define (log-beta-function alpha len)
    (define (gamma xx)
        (let* ([cof (list 76.18009172947146 -86.50532032941677 24.01409824083091 -1.231739572450155 0.001208650973866179 -0.000005395239384953)]
               [ser 1.000000000190015]
               [tmp (- (+ xx 5.5) (* (+ 0.5 xx) (log (+ xx 5.5))))]
               [ser2 (+ ser (apply + (map / cof (map (lambda(num) (+ num xx)) (list 1.0 2.0 3.0 4.0 5.0 6.0)))))]
            )
        (+ (- tmp) (log (* 2.5066282746310005 (/ ser2 xx))))
        
    ))
    
    (let* ([numer (* len (gamma alpha))]
           [denom (gamma (* len alpha))])
          (- numer denom))
    )


  (define (postprocess-params grammar+params)
    (define (has-prefix? lhs-sym)
      (let* ([name-str (symbol->string lhs-sym)]
             [split_underscore (str-split name-str #\_)])
        (<= 3 (length split_underscore))))

    (define (remove-prefix name)
      (if (has-prefix? name)
        (let* ([name-str (symbol->string name)]
               [split_underscore (str-split name-str #\_)])
          (string->symbol (caddr split_underscore)))
        name))
    (define (strip-off-prefix param-cons-cell)
      (let*
        ([tag (car param-cons-cell)]
         [name (car tag)]
         [id (cadr tag)]
         [num-succs (caddr tag)]
         [param-val (cdr param-cons-cell)])
        `((,(remove-prefix name) ,id ,num-succs) ,param-val)))

    (define (table-entry->param table-entry)
      (cdr table-entry))
    (define (table-entry->nt-name table-entry)
      (caar table-entry))
    (define (table-entry->id table-entry)
      (cadar table-entry))
    (define (table-entry->num-ids table-entry)
      (caddar table-entry))

    (define (my-grammar->nts grammar+params)
      (append (program->abstractions grammar+params)
              (list `(abstraction TopLevel () ,(caddr (program->body grammar+params))))))

    (define (group-by-nt renamed-table)
      (define nt-table (make-hash-table equal?))
      (begin
        (for-each (lambda (entry)
                    (let* ([nt-name (table-entry->nt-name entry)])
                      (if 
                        (hash-table-exists? nt-table nt-name)
                        (hash-table-set! nt-table nt-name (cons entry (hash-table-ref nt-table nt-name)))
                        (hash-table-set! nt-table nt-name (list entry)))))
                  renamed-table)
        ;; (map (lambda (nt-name-entries)
               ;; (let* ([nt-name (car nt-name-entries)]
                      ;; [entries (cdr nt-name-entries)])
                 ;; `(,nt-name ,(sort (lambda (x y) (< (car x) (car y)))
                                   ;; (map (lambda (entry)
                                          ;; `(,(table-entry->id entry) ,(table-entry->param entry)))
                                        ;; entries)))))
               ;; (hash-table->alist nt-table))
        (map (lambda (nt-name-entries)
               (let* ([nt-name (car nt-name-entries)]
                      [entries (cdr nt-name-entries)])
                 `(,nt-name ,(map caadr (sort (lambda (x y) (< (car x) (car y)))
                                   (map (lambda (entry)
                                          `(,(table-entry->id entry) ,(table-entry->param entry)))
                                        entries))))))
               (hash-table->alist nt-table))
        ))
    (let*
      (
       [renamed-table (map strip-off-prefix (grammar->params grammar+params))]
       [grouped-by-nt (group-by-nt renamed-table)]
       [same-order-as-grammar (map (lambda (nt)
                                           ;; (assoc (abstraction->name nt) grouped-by-nt))
                                           (cadr (assoc (abstraction->name nt) grouped-by-nt)))
                                   (my-grammar->nts grammar+params))]
       )
      (begin
        ;; (pretty-print renamed-table)
        (grammar-with-params  grammar+params
                              same-order-as-grammar
                              ))))


  (define (assign-uniform-params grammar)
    (let* ([mk-uniform-param-vector (lambda (succs)
                                      (let* ([factor (log (/ 1.0 (length succs)))])
                                        (map (lambda (i) factor) (iota (length succs)))))]
           [nt-params (map (lambda (nt)
                             (let* ([choices (nt->choices nt)])
                               (mk-uniform-param-vector choices)))
                           (grammar->nts grammar))])
      (grammar-with-params grammar nt-params)))

  (define (grammar+params->rule-table grammar+params)
    (let* ([rule-table 
             (make-hash-table equal?)]
           [grammar+tied-params (grammar->tied-params grammar+params)]
           [idx-param-choice->key-val (lambda (lhs-sym idx num-choices param)
                                        `((,lhs-sym ,idx ,num-choices) ,param))]

           )
      (begin
        (map (lambda (nt)
               (let* ([choices (nt->choices nt)]
                      [num-choices (length choices)]
                      [valid-choices (filter (lambda (t) (not (equal? t '(TopLevel)))) choices)]
                      [name (nt->name nt)])
                 (map (lambda (idx choice)
                        (let* ([param (cadr choice)]
                               [kv (idx-param-choice->key-val
                                     name
                                     idx
                                     num-choices
                                     param)]
                               )
                          (hash-table-set!
                            rule-table
                            (car kv)
                            (cadr kv))))
                      (iota (length valid-choices) )
                      valid-choices)))
             (grammar->nts grammar+tied-params))
        rule-table
        )))

  (define (eval-likelihood data grammar+params)
    (let* ([rule-table (grammar+params->rule-table grammar+params)]
           [inversion-results (car (batch-run-inversion (list grammar+params) data))])
      (cond [(contains? '() inversion-results) -inf.0]
            [else
              (let* ([charts (map reformat-exec-chart inversion-results)])
                (grammar->log-likelihood-from-existing-table rule-table charts))])))


  (define (single-data-grammar->likelihood data grammar)
    (cond [(has-params? grammar)
           (eval-likelihood data grammar)]
          [else
            (eval-likelihood data (assign-uniform-params grammar))]))

  (define (populate-stats data grammar+parameters)
    (let* ([unweighted-likelihood (single-data-grammar->likelihood data grammar+parameters)]
           [unweighted-prior (grammar-prior 0.8 grammar+parameters)]
           [posterior (+ (* 1.0 unweighted-likelihood) (* 1.0 unweighted-prior))]
           [description-length (grammar-size grammar+parameters)])
      (grammar-with-stats 
        grammar+parameters
        `(stats
           (posterior ,posterior)
           (likelihood+weight ,unweighted-likelihood 1.0)
           (prior+weight ,unweighted-prior 1.0)
           (desc-length ,description-length)
           (dirichlet-prior ,(grammar-dirichlet-prior 0.8 grammar+parameters))
           ))))


  (define (batch-data-grammar->posterior data grammars . params)

           ;; (define (iterator charts 
           ;;                   grammars 
           ;;                   parameterized-grammar+scores)
           ;;   (cond [(null? grammars) (reverse parameterized-grammar+scores)]

           ;;         [else (let* ([likelihood-weight (cond [(null? params) 1.0]
           ;;                                               [else (car params)])]
           ;;                      [prior-weight (cond [(null? params) 1.0]
           ;;                                          [else (cadr params)])]
           ;;                      [prior-parameter (cond [(= 3 (length params)) (caddr params)]
           ;;                                             [else 1.0])]
           ;;                      )

           ;;                 (let* (
           ;;                        ;; [db (print "beginning parameter estimation")]
           ;;                        [likelihood-parameters (train-parameters (map reformat-exec-chart (car charts)))]
           ;;                        ;; [db (print "done with parameter estimation")]
           ;;                        [likelihood (car likelihood-parameters)]
           ;;                        [params (cadr likelihood-parameters)]
           ;;                        [grammar+parameters (postprocess-params (add-params params (car grammars)))]
           ;;                        [prior (* prior-weight (grammar-prior prior-parameter grammar+parameters))] ;; Prior needs to be calculated here instead; need parameters to be there
           ;;                        )

           ;;                   (iterator (cdr charts) 
           ;;                             (cdr grammars) 
           ;;                             (cons (list 
           ;;                                     grammar+parameters
           ;;                                     (+ likelihood prior)) parameterized-grammar+scores))))]))

           (let* ([all-charts (if (null? grammars) '()
                                (batch-run-inversion grammars data))]
                  [parameterized-grammar+scores
                    (map (lambda (grammar-charts)
                           (let* ([grammar (car grammar-charts)]
                                  [charts (cadr grammar-charts)])
                             (let* ([likelihood-weight (cond [(null? params) 1.0]
                                                             [else (car params)])]
                                    [prior-weight (cond [(null? params) 1.0]
                                                        [else (cadr params)])]
                                    ;; [db (pretty-print (list "likelihood prior weight" likelihood-weight prior-weight))]
                                    [prior-parameter (cond [(= 3 (length params)) (caddr params)]
                                                           [else 1.0])]
                                    )

                               (let* (

                                      [likelihood-parameters (train-parameters (map reformat-exec-chart charts))]
                                      [unweighted-likelihood (car likelihood-parameters)]
                                      [likelihood (* likelihood-weight unweighted-likelihood)]
                                      [params (cadr likelihood-parameters)]
                                      [grammar+parameters (assign-uniform-params (postprocess-params (grammar-with-params grammar params)))]
                                      [description-length (grammar-size grammar)]
                                      [unweighted-prior (grammar-prior prior-parameter grammar+parameters)]
                                      [prior (* prior-weight unweighted-prior)]
                                      [posterior (+ likelihood prior)]
                                      [stats `(stats
                                                (posterior ,posterior)
                                                (likelihood+weight ,unweighted-likelihood ,likelihood-weight)
                                                (prior+weight ,unweighted-prior ,prior-weight)
                                                (desc-length ,description-length)
                                                (dirichlet-prior ,(grammar-dirichlet-prior prior-parameter grammar+parameters))
                                                )]
                                      )
                                 (list (grammar-with-stats grammar+parameters stats) 
                                       posterior)))))
                         (zip grammars all-charts))]
                  ;; [db (print "parallel computation of grammar+parameters finished")]
                  ;;[parameterized-grammar+scores (iterator all-charts grammars '())]
                  )
             (begin ;; (print "batch scores: ~s" scores)
               parameterized-grammar+scores)))
  )
