(library (grammar-likelihood)
  (export
    batch-data-grammar->posterior
    grammar-prior
    )
  (import
    (except (rnrs) string-hash string-ci-hash)
    (_srfi :1)
    (_srfi :69)
    (program-likelihood)
    (chart-parsing)
    (program)
    (parameter-estimation)
    (printing)
    (util))
  (define (grammar-size prog)
    (+ (apply + (map (lambda (abstr) (+ 1  ;; + 1: The "separator" symbol between nonterminals basically encourages merging
                                        (cond [(eq? 'choose (car (abstraction->pattern abstr))) ;; Choose operator does not count, so subtract 1 for using choose
                                               (- (sexpr-size (abstraction->pattern abstr)) 1)]
                                              [else (sexpr-size (abstraction->pattern abstr))])))
                     (program->abstractions prog)))
       (sexpr-size (program->body prog))))

  (define (log-prob-normalize probs)
    (let* ([denom (log (apply + (map exp probs)))])
    (map (lambda (n) (- n denom)) probs)))

  (define (grammar-prior prior-parameter prog)
    (let* (
        [prior-struct (- (grammar-size prog))]
        [prod-param (* (- prior-parameter 1) (apply + (log-prob-normalize (concatenate (cadddr prog)))))]
        [prob-param (- prod-param (log-beta-function prior-parameter (length (concatenate (cadddr prog)))))])
    (+ prior-struct prob-param)))

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

  (define (add-params params grammar)
    `(program
       ,(program->abstractions grammar)
       ,(program->body grammar)
       ,params))

  (define grammar->params cadddr)

  (define (postprocess-params grammar+params)
    ;; From http://schemecookbook.org/Cookbook/StringSplit
    (define (str-split str ch)
      (let ((len (string-length str)))
        (letrec
          ((split
             (lambda (a b)
               (cond
                 ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
                 ((char=? ch (string-ref str b)) (if (= a b)
                                                   (split (+ 1 a) (+ 1 b))
                                                   (cons (substring str a b) (split b b))))
                 (else (split a (+ 1 b)))))))
          (split 0 0))))
    (define (remove-prefix name)
      (let* ([name-str (symbol->string name)]
             [split_underscore (str-split name-str #\_)])
        (string->symbol (caddr split_underscore))))
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
        (add-params same-order-as-grammar grammar+params))))

  (define (batch-data-grammar->posterior data progs . params)

           (define (iterator charts 
                             grammars 
                             parameterized-grammar+scores)
             (cond [(null? grammars) (reverse parameterized-grammar+scores)]

                   [else (let* ([likelihood-weight (cond [(null? params) 1.0]
                                                         [else (car params)])]
                                [prior-weight (cond [(null? params) 1.0]
                                                    [else (cadr params)])]
                                [prior-parameter (cond [(= 3 (length params)) (caddr params)]
                                                       [else 1.0])]
                                )

                           (let* (
                                  [likelihood-parameters (train-parameters (map reformat-exec-chart (car charts)))]
                                  [likelihood (car likelihood-parameters)]
                                  [params (cadr likelihood-parameters)]
                                  [grammar+parameters (postprocess-params (add-params params (car grammars)))]
                                  [prior (* prior-weight (grammar-prior prior-parameter grammar+parameters))] ;; Prior needs to be calculated here instead; need parameters to be there
                                  )

                             (iterator (cdr charts) 
                                       (cdr grammars) 
                                       (cons (list 
                                               grammar+parameters
                                               (+ likelihood prior)) parameterized-grammar+scores))))]))

           (let* ([all-charts (if (null? progs) '()
                                (batch-run-inversion progs data))]
                  [parameterized-grammar+scores (iterator all-charts progs '())])
             (begin ;; (print "batch scores: ~s" scores)
               parameterized-grammar+scores)))
  )
