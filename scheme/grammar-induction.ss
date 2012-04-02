(library (grammar-induction)
         (export gi-bmm
                 grammar-sort
                 delete-duplicates-by-hash
                 lgcg
                 lgcg-generic
                 mgcg)

         (import 
           (except (rnrs) string-hash string-ci-hash) 
           (sxml2scfg)
           (beam-learning)
           (printing)
           (util)
           (program)
           (grammars)
           (combinations)
           (_srfi :1)
           (_srfi :69)
           (grammar-likelihood)
           (sym)

           (profiling)

           (forkmap)

           (_srfi :67)
           )

         (define (lgcg-generic data nt-pred)
           (let* ([init-prog (sxmls->initial-program nt-pred data #t)])
             (populate-stats data (assign-uniform-params (make-grammar (program->abstractions init-prog)
                                                                       (program->body init-prog))))))
         
         (define (lgcg data)
           (let* ([init-prog (sxmls->initial-program elt-pred data #t)])
             (make-grammar
               (program->abstractions init-prog)
               (program->body init-prog))))

         (define (mgcg data)
           (letrec* ([start (lgcg data)]
                     [loop (lambda (grammar)
                             (let* ([next-grammar (next-merge grammar #f)])
                               (cond [(null? next-grammar) grammar]
                                     [else (loop next-grammar)])))])
                    (loop start)))


         ;; (define (make-context-compatibility data)
         ;;   ;; given: nt1, nt2 produce the same geometric element
         ;;   ;; 1. compute insides of nt1, nt2 --- the unique sub-parts of exemplars that are produced by nt1, nt2
         ;;   ;; 2. for each inside, compute the outsides --- the 
         ;;   (lambda (nt1 nt2)
         ;;     '()))


         (define (nt-deletions prog)

           ;; where
           (define (all-rule-deletes nt prog)
             (map (lambda (i)
                    (begin
                      ;; (print "removing the ~s rule of ~s" i (abstraction->name nt))
                      (delete-ith-rule i nt prog)))
                  (iota (nt->num-rules nt))))

           (define (nt->num-rules nt)
             (cond [(null? (abstraction->pattern nt)) 0]
                   [else 
                     (length (nt->choices nt))]))

           (define (nt->rules nt)
             (let* ([body (abstraction->pattern nt)])
               (cond [(eq? 'choose (car body))
                      (cdr body)]
                     [else (list body)])))

           (define (empty? nt)
             (let* ([body (abstraction->pattern nt)])
               (cond [(equal? '(choose) body) #t]
                     [(equal? '() body) #t]
                     [else #f])))

           (define (delete-ith-rule i nt prog)
             (let* ([program-after-deletion
                      (program->replace-abstraction
                        prog
                        (nt-without-rule-at i nt))])
               (cleanup-after-deletion program-after-deletion)))

           (define (nt-without-rule-at i nt)
             (make-named-abstraction
               (abstraction->name nt)
               (cond [(choice? (abstraction->pattern nt))
                      `(choose ,@(list-remove-at i (nt->choices nt)))]
                     ;; we only have one rule, so return '()
                     [else '()])
               '()))

           (define (has-application? nt sexpr)
             (not (null? (deep-find-all (lambda (t) (equal? `(,(abstraction->name nt)) t))
                                        sexpr))))

           (define (get-incident-rules target-nts prog)
             (define all-nts (program->abstractions prog))
             (define (occurrences-in nt)
               (let* (
                      ;; [db (begin (print "in get-incident-rules: target-nt-names: ~s" (map abstraction->name target-nts))
                      ;; (print "nt in question:")
                      ;; (pretty-print nt))]
                      [body (nt->rules nt)]
                      [idx-body (zip (iota (length body))
                                     body)]

                      [answer (list
                                (abstraction->name nt)
                                (map car (delete-duplicates
                                           (concatenate
                                             (map (lambda (target-nt)
                                                    (filter (lambda (idx-pattern)
                                                              (has-application? target-nt (cadr idx-pattern)))
                                                            idx-body)) target-nts)))))]

                      ;; [db (pretty-print answer)]

                      )
                 answer
                 ))
             (filter (lambda (result)
                       (not (null? (cadr result))))
                     (map occurrences-in all-nts)))

           (define (remove-several-rules nt-idxss prog)
             (let* ([new-nts
                      (map (lambda (abstr)
                             (cond [(assq (abstraction->name abstr) nt-idxss)
                                    (let* ([idxs-to-remove (cadr (assq (abstraction->name abstr) nt-idxss))])
                                      (make-named-abstraction
                                        (abstraction->name abstr)
                                        (cond [(choice? abstr)
                                               `(choose ,@(list-remove-at-several idxs-to-remove (nt->choices abstr)))]
                                              [else (cond [(null? idxs-to-remove) (abstraction->pattern abstr)]
                                                          [else '()])]
                                              )
                                        '()))]
                                   [else abstr]))
                           (program->abstractions prog))])
               (make-grammar new-nts (program->body prog))))



           ;; after deleting a rule, we might end up with isolated/empty nonterminals, which should also be removed.
           (define (cleanup-after-deletion prog)
             (let* (
                    ;; [db (begin  (print "program to clean up:") (pretty-print prog))]
                    [isolated-nts (get-unused-nts prog)]

                    ;; [db (begin (print "isolated nts:") (pretty-print isolated-nts))]

                    [prog2 (remove-several-nts isolated-nts prog)]

                    ;; [db (begin (print "prog2:") (pretty-print prog2))]

                    [empty-nts (get-empty-nts prog)]

                    ;; [db (begin (print "empty nts:") (pretty-print empty-nts))]

                    [prog3 (remove-several-nts empty-nts prog2)]
                    ;; [db (begin (print "prog3:") (pretty-print prog3))]
                    [next-rules-to-remove
                      (get-incident-rules empty-nts prog3)] ;; returns data structures containing: (list nt-name idx)
                    ;; [db (begin (print "next rules to remove:") (pretty-print next-rules-to-remove))]
                    [final-prog (remove-from-body empty-nts (remove-several-rules next-rules-to-remove prog3))]
                    ;; [db (begin (print "final prog:") (pretty-print final-prog))]
                    )
               (cond [(and (null? empty-nts) (null? isolated-nts))
                      prog]
                     [else (cleanup-after-deletion final-prog)])))

           (define (get-unused-nts prog)
             (define (unused? nt)
               (let* ([nts (program->abstractions prog)]
                      [bodies (map abstraction->pattern nts)])
                 (and
                   (not (has-application? nt bodies))
                   (not (has-application? nt (program->body prog))))))

             (filter (lambda (abstr)
                       (unused? abstr))
                     (program->abstractions prog)))

           (define (remove-from-body target-nts prog)
             (let* ([target-names (map abstraction->name target-nts)])
               (make-grammar
                 (program->abstractions prog)
                 `(lambda () (choose ,@(filter (lambda (nt)
                                                 (not (contains? (car nt) target-names)))
                                               (cdr (caddr (program->body prog)))))))))

           (define (remove-several-nts nts prog)
             (let* (
                    [names-to-remove (map abstraction->name nts)]
                    [abstractions-after (filter (lambda (abstr)
                                                  (not (contains? (abstraction->name abstr) names-to-remove)))
                                                (program->abstractions prog))])
               (grammar-with-new-nts+body
                 abstractions-after
                 (program->body prog))))

           (define (get-empty-nts prog)
             (filter empty? (program->abstractions prog)))

           (let* ([answers
                    (filter (lambda (prog) ;; Reject empty programs
                              (not (equal? '(lambda () (choose))
                                           (program->body prog))))
                            (concatenate (map (lambda (nt) (all-rule-deletes nt prog))
                                              (program->abstractions prog))))])
             (begin 
               ;; (print "Deletions:")
               ;; (pretty-print answers)
               answers))


           )

         (define 
           (next-merge prog keep-history?)

           (define grammar-constructor
             (cond [keep-history? grammar-with-new-nts+body+history]
                   [else grammar-with-new-nts+body]))
           (define 
             (nt-pair->merge f1f2)
             (let* ([none (set-indices-floor! prog)]

                    [new-abstraction-name (sym (func-symbol))]

                    [to-remove (map abstraction->name f1f2)]

                    [transform-pattern (let* (
                                              [transform-expr (lambda (e) `(,new-abstraction-name))]
                                              [transform-pred (lambda (e) (and (non-empty-list? e) (contains? (car e) to-remove)))])
                                         (lambda (e) (sexp-search transform-pred transform-expr e))
                                         )]

                    [transform-old-abstraction (lambda (a) (make-named-abstraction (abstraction->name a)
                                                                                   (transform-pattern (abstraction->pattern a))
                                                                                   (abstraction->vars a)))]

                    [new-program-body (transform-pattern (program->body prog))]
                    [new-program-abstractions (filter (lambda (a) (not (contains? (abstraction->name a) to-remove)))
                                                      (map transform-old-abstraction (program->abstractions prog)))]

                    [f1f2* (map transform-old-abstraction f1f2)]

                    ;; todo: (delete-duplicate-choices <pattern>)
                    ;; searches for occurrences of choose and deletes duplicate successors

                    [new-bodies (delete-duplicates (append (nt->choices (first f1f2*)) (nt->choices (second f1f2*))))]

                    [new-abstraction (make-named-abstraction new-abstraction-name
                                                             (cond [(= 1 (length new-bodies)) (car new-bodies)]
                                                                   [else `(choose ,@new-bodies)])
                                                             '())]
                    )

               (remove-duplicate-choices (grammar-constructor prog (cons new-abstraction
                                                                                  new-program-abstractions)
                                                                            new-program-body
                                                                            ))
               ))

           (define (elem->url elem)
             (cadr (cadr elem)))

           (define (elem->sym elem)
             (cadr elem))

           (define (same-type? f1f2)
             (begin 
               (equal? (elem->sym (car (nt->choices (car f1f2))))
                       (elem->sym (car (nt->choices (cadr f1f2)))))))

           (let* ([compatible-nts (filter same-type? (select-k-subsets 2 (program->abstractions prog)))]
                  [possible-merge (cond [(null? compatible-nts) '()]
                                        [else (nt-pair->merge (car compatible-nts))])])
             possible-merge))

         (define (remove-duplicate-choices grammar)
           (define (remove-duplicates-for-nt nt)
             (let* ([new-choices (delete-duplicates (nt->choices nt))])
               (cond [(= 1 (length new-choices))
                      (make-named-abstraction
                        (abstraction->name nt)
                        (car new-choices)
                        '())]
                     [else
                       (make-named-abstraction
                         (abstraction->name nt)
                         `(choose ,@new-choices)
                         '())])))

           (define (remove-top-level-duplicates thunk)
             (let* ([new-choices
                      (delete-duplicates (cdr (caddr thunk)))])
               `(lambda () (choose ,@new-choices))))
           (grammar-with-new-nts+body
             grammar
             (map remove-duplicates-for-nt (program->abstractions grammar))
             (remove-top-level-duplicates (program->body grammar))
             ))
        (define 
          (sample-merges prog throw-out-factor)
          (define grammar-constructor grammar-with-new-nts+body)
          (define 
            (nt-pair->merge f1f2)
            (let* ([none (set-indices-floor! prog)]

                   [new-abstraction-name (sym (func-symbol))]

                   [to-remove (map abstraction->name f1f2)]

                   [transform-pattern (let* (
                                             [transform-expr (lambda (e) `(,new-abstraction-name))]
                                             [transform-pred (lambda (e) (and (non-empty-list? e) (contains? (car e) to-remove)))])
                                        (lambda (e) (sexp-search transform-pred transform-expr e))
                                        )]

                   [transform-old-abstraction (lambda (a) (make-named-abstraction (abstraction->name a)
                                                                                  (transform-pattern (abstraction->pattern a))
                                                                                  (abstraction->vars a)))]

                   [new-program-body (transform-pattern (program->body prog))]
                   [new-program-abstractions (filter (lambda (a) (not (contains? (abstraction->name a) to-remove)))
                                                     (map transform-old-abstraction (program->abstractions prog)))]

                   [f1f2* (map transform-old-abstraction f1f2)]

                   ;; todo: (delete-duplicate-choices <pattern>)
                   ;; searches for occurrences of choose and deletes duplicate successors

                   [new-bodies (delete-duplicates (append (nt->choices (first f1f2*)) (nt->choices (second f1f2*))))]

                   [new-abstraction (make-named-abstraction new-abstraction-name
                                                            (cond [(= 1 (length new-bodies)) (car new-bodies)]
                                                                  [else `(choose ,@new-bodies)])
                                                            '())]
                   )

              (remove-duplicate-choices
                (grammar-constructor
                  prog
                  (cons new-abstraction
                        new-program-abstractions)
                  new-program-body)
                )
              ))

          (define (elem->url elem)
            (cadr (cadr elem)))

          (define (elem->sym elem)
            (cadr elem))

          (define (same-type? f1f2)
            (begin 
              (equal? (elem->sym (car (nt->choices (car f1f2))))
                      (elem->sym (car (nt->choices (cadr f1f2)))))))

          (let* ([possible-merges (map nt-pair->merge
                                       (rnd-drop-list (- 1.0 throw-out-factor)
                                                      (filter same-type? 
                                                              (select-k-subsets 2 (program->abstractions prog)))))])
                         possible-merges))
         (define 
           (pairwise-nt-merges prog num-threads keep-history?)
           (define grammar-constructor
             (cond [keep-history? grammar-with-new-nts+body+history]
                   [else grammar-with-new-nts+body]))
           (define 
             (nt-pair->merge f1f2)
             (let* ([none (set-indices-floor! prog)]

                    [new-abstraction-name (sym (func-symbol))]

                    [to-remove (map abstraction->name f1f2)]

                    [transform-pattern (let* (
                                              [transform-expr (lambda (e) `(,new-abstraction-name))]
                                              [transform-pred (lambda (e) (and (non-empty-list? e) (contains? (car e) to-remove)))])
                                         (lambda (e) (sexp-search transform-pred transform-expr e))
                                         )]

                    [transform-old-abstraction (lambda (a) (make-named-abstraction (abstraction->name a)
                                                                                   (transform-pattern (abstraction->pattern a))
                                                                                   (abstraction->vars a)))]

                    [new-program-body (transform-pattern (program->body prog))]
                    [new-program-abstractions (filter (lambda (a) (not (contains? (abstraction->name a) to-remove)))
                                                      (map transform-old-abstraction (program->abstractions prog)))]

                    [f1f2* (map transform-old-abstraction f1f2)]

                    ;; todo: (delete-duplicate-choices <pattern>)
                    ;; searches for occurrences of choose and deletes duplicate successors

                    [new-bodies (delete-duplicates (append (nt->choices (first f1f2*)) (nt->choices (second f1f2*))))]

                    [new-abstraction (make-named-abstraction new-abstraction-name
                                                             (cond [(= 1 (length new-bodies)) (car new-bodies)]
                                                                   [else `(choose ,@new-bodies)])
                                                             '())]
                    )

               (remove-duplicate-choices
                 (grammar-constructor
                   prog
                   (cons new-abstraction
                         new-program-abstractions)
                   new-program-body)
                 )
               ))

           (define (elem->url elem)
             (cadr (cadr elem)))

           (define (elem->sym elem)
             (cadr elem))

           (define (same-type? f1f2)
             (begin 
               (equal? (elem->sym (car (nt->choices (car f1f2))))
                       (elem->sym (car (nt->choices (cadr f1f2)))))))

           (let* ([possible-merges (forkmap nt-pair->merge
                                            (filter same-type? 
                                                    (select-k-subsets 2 (program->abstractions prog)))
                                            num-threads
                                            )])
             (begin 
               possible-merges)))
(define 
  (serial-pairwise-nt-merges prog keep-history?)
  (define grammar-constructor
    (cond [keep-history? grammar-with-new-nts+body+history]
          [else grammar-with-new-nts+body]))
  (define 
    (nt-pair->merge f1f2)
    (let* ([none (set-indices-floor! prog)]

           [new-abstraction-name (sym (func-symbol))]

           [to-remove (map abstraction->name f1f2)]

           [transform-pattern (let* (
                                     [transform-expr (lambda (e) `(,new-abstraction-name))]
                                     [transform-pred (lambda (e) (and (non-empty-list? e) (contains? (car e) to-remove)))])
                                (lambda (e) (sexp-search transform-pred transform-expr e))
                                )]

           [transform-old-abstraction (lambda (a) (make-named-abstraction (abstraction->name a)
                                                                          (transform-pattern (abstraction->pattern a))
                                                                          (abstraction->vars a)))]

           [new-program-body (transform-pattern (program->body prog))]
           [new-program-abstractions (filter (lambda (a) (not (contains? (abstraction->name a) to-remove)))
                                             (map transform-old-abstraction (program->abstractions prog)))]

           [f1f2* (map transform-old-abstraction f1f2)]
           [new-bodies (delete-duplicates (append (nt->choices (first f1f2*)) (nt->choices (second f1f2*))))]
           [new-abstraction (make-named-abstraction new-abstraction-name
                                                    (cond [(= 1 (length new-bodies)) (car new-bodies)]
                                                          [else `(choose ,@new-bodies)])
                                                    '())]
           )

      (remove-duplicate-choices
        (grammar-constructor
          prog
          (cons new-abstraction
                new-program-abstractions)
          new-program-body)
        )
      ))

  (define (elem->url elem)
    (cadr (cadr elem)))

  (define (elem->sym elem)
    (cadr elem))

  (define (same-type? f1f2)
    (begin 
      (equal? (elem->sym (car (nt->choices (car f1f2))))
              (elem->sym (car (nt->choices (cadr f1f2)))))))

  (let* ([possible-merges (map nt-pair->merge
                               (filter same-type? 
                                       (select-k-subsets 2 (program->abstractions prog)))
                               )])
    (begin 
      possible-merges)))

         (define (elt? sym) 
           (and (symbol? sym)
                (eq? 'elem sym)))

         (define elt-pred (lambda (e) (and (not (null? e)) 
                                           (list? e) 
                                           (not (null? (cdr e))) 
                                           (elt? (car e)))))

         (define (delete-duplicates-by-hash hash-fx xs)
           (define my-table (make-hash-table equal?))

           (define (loop acc xs)
             (cond [(null? xs) 
                    (reverse acc)]
                   [else
                     (let* ([pt (car xs)]
                            [hash-val (hash-fx pt)])
                       (if (hash-table-exists? my-table hash-val)
                         (loop acc (cdr xs))
                         (begin
                           (hash-table-set! my-table hash-val 'TAKEN)
                           (loop (cons pt acc) (cdr xs)))))]))

           (loop '() xs))
         
         (define STRATEGY_UNLIMITED 0)
         (define STRATEGY_CONST 1)
         (define STRATEGY_LOCAL 2)
         (define STRATEGY_FULL 3)
         (define STRATEGY_STOCHASTIC 4)
         (define STRATEGY_STOCHASTIC_SINGLE 5)
         (define STRATEGY_NOISY 6)
         (define STRATEGY_MULTI 7)


         (define-opt
           (gi-bmm data stop-number beam-size (optional
                                                (likelihood-weight 1.0)
                                                (prior-weight 1.0)
                                                (prior-parameter 1.0)
                                                (search-strategy STRATEGY_CONST)
                                                (num-threads 8)
                                                (keep-history? #f)
                                                (stop-at-depth '())))
           (define beam-search-strategy 
             (cond [(= STRATEGY_UNLIMITED search-strategy) beam-search-unlimited-fringe]
                   [(= STRATEGY_CONST search-strategy) beam-search-const-mem]
                   [(= STRATEGY_MULTI search-strategy) 
                    (lambda args (apply (curry beam-search-const-mem+multi-expand num-threads) args))]
                   [(= STRATEGY_LOCAL search-strategy) 
                    (lambda args (apply (curry beam-search-local num-threads) args))]
                   [(= STRATEGY_FULL search-strategy) beam-search-full]
                   [(= STRATEGY_STOCHASTIC search-strategy) 
                    (lambda args (apply (curry beam-search-stochastic num-threads) args))]
                   [(= STRATEGY_NOISY search-strategy) 
                    (lambda args (apply (curry beam-search-stochastic num-threads) args))]
                   [(= STRATEGY_STOCHASTIC_SINGLE search-strategy) beam-search-stochastic-single]
                   ))

           (define (grammar->merges prog)
             (begin
               (pairwise-nt-merges prog num-threads keep-history?)
               ))
          (define (grammar->merges-serial prog)
            (begin
              (serial-pairwise-nt-merges prog keep-history?)
              ))
          (define (grammar->incomplete-merges prog)
            (sample-merges prog 0.95))

          (define merge-strategy (cond 
                                   [(= STRATEGY_NOISY search-strategy) grammar->incomplete-merges]
                                   [(= STRATEGY_STOCHASTIC search-strategy) grammar->merges-serial]
                                   [(= STRATEGY_LOCAL search-strategy) grammar->merges-serial]
                                   [(= STRATEGY_MULTI search-strategy) grammar->merges-serial]
                                   [else grammar->merges]))


           (define prog-table (make-hash-table equal?))

           (define (prog->unlabeled prog)

             (define (abstr->num-successors prog abstr)
               (let* ([is-successor? (lambda (expr)
                                       (and (non-empty-list? expr)
                                            (= 1 (length expr))
                                            (contains? (car expr) (map abstraction->name (program->abstractions prog)))))])
                 (length (sexp-search is-successor? (lambda (x) x) (abstraction->pattern abstr)))))

             (let* ([sorted-prog (grammar-sort prog)])
               (sort < (map (curry abstr->num-successors sorted-prog)
                            (program->abstractions sorted-prog)))))

           (define (prog-likelihood->hash prog-likelihood)
             (let* (
                    [prog (car prog-likelihood)]
                    [likelihood (cadr prog-likelihood)]
                    [answer (list likelihood (prog->unlabeled prog)) ]
                    )
               answer))


           (define-timed (fringe->merged-fringe prog-likelihoods) ;; can result in starvation of the beam
                         ;; (delete-duplicates-by-hash prog-likelihood->hash prog-likelihoods))
                         (delete-duplicates-by-hash (lambda (pl) (grammar-sort (car pl))) prog-likelihoods))


                    (define (print-grammar-stats grammar)
             (begin
               (pretty-print grammar)
               (for-each
                 (lambda (stat)
                   (cond [(contains? (car stat) 
                                     '(posterior likelihood+weight prior+weight desc-length dirichlet-prior))

                          (begin
                            (for-each (lambda (x) (display x) (display " ")) stat) 
                            (newline))]
                         [else '()]))
                 (cdr (grammar->stats grammar)))))



           (define (print-stats fringe depth)
             (let ([best-prog (caar fringe)])
               (begin 
                 ;;(print "depth: ~s best program:" depth)
                 ;;(print "posterior: ~s" (cdar fringe))
                 (print-grammar-stats (caar fringe))
                 )))


           (define (depth-stop fringe depth)
             (begin (print-stats fringe depth)
                    (= 0 depth)))

           (define (same-prog-stop limit)
             (define prog-store '())

             (define (add-one-prog-likelihood p) 
               (set! prog-store (cons p prog-store)))

             (define (reached-limit?)
               (let* ([tail (max-take prog-store limit)]
                      [tail-len (length (delete-duplicates tail))])
                 (begin (print "limit: ~s" limit)
                        (print "num unique grammars in limit: ~s" tail-len)
                        (cond [(>= (length tail) limit) (= 1 tail-len)]
                              [else #f]))))

             (lambda (fringe depth)
               (begin (print-stats fringe depth)
                      (add-one-prog-likelihood (car fringe))
                      (reached-limit?))))


           (define (score+update-grammars grammars)
             (let* ([grouped-grammars (split-into num-threads grammars)])
               (concatenate
                 (forkmap-direct 
                   (lambda (gs)
                     (batch-data-grammar->posterior data gs likelihood-weight prior-weight prior-parameter))
                   grouped-grammars))))

           (define (prefilter-lex-equal-grammars grammars)
             (delete-duplicates-by-hash (lambda (x) x) grammars))

           (let* ([initial-prog (lgcg data)]
                  [initial-fringe-pt (score+update-grammars (list initial-prog))]
                  [learned-program (beam-search-strategy
                                     initial-fringe-pt
                                     (car initial-fringe-pt)
                                     beam-size
                                     merge-strategy
                                     prefilter-lex-equal-grammars
                                     score+update-grammars
                                     fringe->merged-fringe
                                     (if (not (null? stop-at-depth)) depth-stop (same-prog-stop stop-number)))])
             learned-program))

         (define (renormalize-names grammar)
           (let* ([counter 0]
                  [nt-names (map abstraction->name (program->abstractions grammar))]
                  [new-sym (lambda () (let* ([answer (string->symbol (string-append "F" (number->string counter)))])
                                        (begin
                                          (set! counter (+ 1 counter))
                                          answer)))]
                  [nt-name-table (make-hash-table equal?)]
                  [retrieve-name 
                    (lambda (t)
                      (cond [(contains? t nt-names)
                             (hash-table-ref nt-name-table t
                                             (lambda ()
                                               (let* ([answer (new-sym)])
                                                 (hash-table-set! nt-name-table t answer)
                                                 answer)))]
                            [else t]))])
             (sexp-walk retrieve-name grammar)))

         (define (grammar-sort grammar)
           (define (default-< x y)
             (= -1 (default-compare x y)))

           (define (sort-element elt)
             (let* ([recover-elt (lambda (body) `(,(car elt) ,(cadr elt) ,@body))]
                    [body (cddr elt)])
               (recover-elt (sort (lambda (x y)
                                    (default-< (cadr x) (cadr y)))
                                  body))))

           (define (sort-nt-body nt)
             (let* ([body (abstraction->pattern nt)])
               (cond [(eq? 'choose (car body))
                      (make-named-abstraction
                        (abstraction->name nt)
                        `(choose ,@(sort (lambda (x y)
                                           (default-< (cddr x) (cddr y)))
                                         (map sort-element (cdr body))))
                        '())]
                     [else (make-named-abstraction
                             (abstraction->name nt)
                             (sort-element body)
                             '())])))
           (define (sort-nts nts)
             (sort 
               (lambda (nt1 nt2)
                 (default-< (abstraction->pattern nt1)
                            (abstraction->pattern nt2)))
               (map sort-nt-body nts)))


           (let ([nts (program->abstractions grammar)]
                 [body (program->body grammar)])
             (renormalize-names
               (make-grammar
                 (sort-nts nts)
                 body))))
         )
