(library (grammar-induction)
         (export gi-bmm
                 gi-bmm-prototype)

         (import 
           (except (rnrs) string-hash string-ci-hash) 
           (sxml2scfg)
           (beam-learning)
           (printing)
           (util)
           (program)
           (combinations)
           (_srfi :1)
           (_srfi :69)
           (program-likelihood)
           (sym)

           (profiling)

           (_srfi :67)
           )

        (define (choice? body) (eq? 'choose (car body)))
        (define (nt->choices nt)
          (let* ([main-body (abstraction->pattern nt)])
            (cond [(choice? main-body) (cdr main-body)]
                  [else (list main-body)])))

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
                      `(choose (list-remove-at i (nt->choices nt)))]
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
                      ;; [db (begin (print "in get-incident-rules: target-nt-names: ~s" (map abstraction->name target-nts)))]
                      [body (nt->rules nt)]
                      [idx-body (zip (iota (length body))
                                     body)])
                 (list
                   (abstraction->name nt)
                   (map car (delete-duplicates
                              (concatenate
                                (map (lambda (target-nt)
                                       (filter (lambda (idx-pattern)
                                                 (has-application? target-nt (cadr idx-pattern)))
                                               idx-body)) target-nts)))))))
             (filter (lambda (result)
                       (null? (cadr result)))
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
               (make-program new-nts (program->body prog))))



           ;; after deleting a rule, we might end up with isolated/empty nonterminals, which should also be removed.
           (define (cleanup-after-deletion prog)
             (let* ([isolated-nts (get-unused-nts prog)]

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
               (make-program
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
               (make-program
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
           (pairwise-nt-merges prog)
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

               (make-program (cons new-abstraction
                                   new-program-abstractions)
                             new-program-body)))

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
                                                (select-k-subsets 2 (program->abstractions prog))))])
             (begin (print "# possible merges: ~s" (length possible-merges))
                    possible-merges)))

         (define (elt? sym) 
           (and (symbol? sym)
                (eq? 'elem sym)))

         (define elt-pred (lambda (e) (and (not (null? e)) 
                                           (list? e) 
                                           (not (null? (cdr e))) 
                                           (elt? (car e)))))

         ;; gi-bmm: the 'stable' version
         (define
           (gi-bmm data beam-size likelihood-weight prior-weight . stop-at-depth)

           ;; TODO: Which one is right?
           (define prog-table (make-hash-table equal?))

           (define (prog->unlabeled prog)

             (define (abstr->num-successors prog abstr)
               (let* ([is-successor? (lambda (expr)
                                       (and (non-empty-list? expr)
                                            (= 1 (length expr))
                                            (contains? (car expr) (map abstraction->name (program->abstractions prog)))))])
                 (length (sexp-search is-successor? (lambda (x) x) (abstraction->pattern abstr)))))

             ;; equivalent to grammar-sort beforehand, but faster; grammar-sort
             ;; would have ordered nonterminals by structure of their RHS
             ;; anyway
             
             (sort < (map (curry abstr->num-successors prog)
                          (program->abstractions prog))))

           (define (prog-likelihood->hash prog-likelihood)
             (let* (
                    [prog (car prog-likelihood)]
                    [likelihood (cadr prog-likelihood)]
                    [answer (list likelihood (prog->unlabeled prog)) ]
                    )
               answer))


           (define-timed (fringe->merged-fringe prog-likelihoods) ;; can result in starvation of the beam
                         (delete-duplicates-by-hash prog-likelihood->hash prog-likelihoods))

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

           (define (program->transforms prog)
             (begin
               ;; (print "PROG: ~s" prog)
               ;; (cons prog (pairwise-nt-merges prog))
               (pairwise-nt-merges prog)
               ))

           (define (program->log-posterior prog)
             (apply + (map (lambda (d) (data-program->log-posterior d prog likelihood-weight prior-weight))
                           data)))


           (define (print-stats fringe depth)
             (let ([best-prog (caar fringe)])
               (begin (print "depth: ~s best program:" depth)
                      (print "posterior: ~s" (program->log-posterior best-prog))
                      (pretty-print (car fringe)))))


           (define (depth-stop fringe depth)
             (begin (print-stats fringe depth)
                    (= 0 depth)))



           (define (same-prog-stop limit)
                         (define prog-store '())

                         (define (add-one-prog-likelihood p) 
                           ;; distinguish between lexicographically-equivalent grammars
                           (let ([grammar (grammar-sort (car p))]
                                 [likelihood (cadr p)])
                             (set! prog-store (cons (list grammar likelihood) prog-store))))

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

           (let* ([initial-prog (sxmls->initial-program elt-pred data)]
                  [learned-program (beam-search3 (zip 
                                                   (list initial-prog)
                                                   (score-programs (list initial-prog)))
                                                 (list initial-prog (car (score-programs (list initial-prog))))
                                                 beam-size ;; (if (not (null? stop-at-depth)) (car stop-at-depth) 0)
                                                 program->transforms
                                                 score-programs
                                                 fringe->merged-fringe
                                                 ;; (lambda (progs) progs)
                                                 (if (not (null? stop-at-depth)) depth-stop (same-prog-stop 20)))])
             learned-program))

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
             (make-program
               (sort-nts nts)
               body)))


         ;; gi-bmm-prototype: experimenting with different versions of the algorithm
         (define
           (gi-bmm-prototype data beam-size likelihood-weight prior-weight . stop-at-depth)

           ;; TODO: Which one is right?
           (define prog-table (make-hash-table equal?))

           (define (prog->unlabeled prog)

             (define (abstr->num-successors prog abstr)
               (let* ([is-successor? (lambda (expr)
                                       (and (non-empty-list? expr)
                                            (= 1 (length expr))
                                            (contains? (car expr) (map abstraction->name (program->abstractions prog)))))])
                 (length (sexp-search is-successor? (lambda (x) x) (abstraction->pattern abstr)))))

             ;; equivalent to grammar-sort beforehand, but faster; grammar-sort
             ;; would have ordered nonterminals by structure of their RHS
             ;; anyway
             
             (sort < (map (curry abstr->num-successors prog)
                          (program->abstractions prog))))

           (define (prog-likelihood->hash prog-likelihood)
             (let* (
                    [prog (car prog-likelihood)]
                    [likelihood (let* ([val (cadr prog-likelihood)])
                                  (cond [(= -inf.0 val) 'log-zero]
                                        [else val]))]
                    [answer (list likelihood (prog->unlabeled prog)) ]
                    )
               answer))


           (define-timed (fringe->merged-fringe prog-likelihoods) ;; can result in starvation of the beam
                         (delete-duplicates-by-hash prog-likelihood->hash prog-likelihoods))

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

           (define (program->transforms prog)
             (begin
               ;; (print "PROG: ~s" prog)
               ;; (cons prog (pairwise-nt-merges prog))
               (append
                 (pairwise-nt-merges prog)
                 (nt-deletions prog)
                 )
               ))

           ;; Note: sum over all examples, not multiply, to handle noisy data
           (define (program->log-posterior prog)
             (apply log-prob-sum2 
                    (map (lambda (d) (data-program->log-posterior d prog likelihood-weight prior-weight))
                         data)))


           (define (print-stats fringe depth)
             (let ([best-prog (caar fringe)])
               (begin (print "depth: ~s best program:" depth)
                      (print "posterior: ~s" (program->log-posterior best-prog))
                      (pretty-print (car fringe)))))


           (define (depth-stop fringe depth)
             (begin (print-stats fringe depth)
                    (= 0 depth)))



           (define (same-prog-stop limit)
                         (define prog-store '())

                         (define (add-one-prog-likelihood p) 
                           ;; distinguish between lexicographically-equivalent grammars
                           (let ([grammar (grammar-sort (car p))]
                                 [likelihood (cadr p)])
                             (set! prog-store (cons (list grammar likelihood) prog-store))))

                         (define (reached-limit?)
                           (let ([tail (max-take prog-store limit)])
                             (cond [(>= (length tail) limit) (= 1 (length (delete-duplicates tail)))]
                                   [else #f])))

                         (lambda (fringe depth)
                           (begin (print-stats fringe depth)
                                  (add-one-prog-likelihood (car fringe))
                                  (reached-limit?))))

           (define (score-programs progs)
                         (batch-data-program->sum-posterior data progs likelihood-weight prior-weight))

           (let* ([initial-prog (sxmls->initial-program elt-pred data)]
                  [learned-program (beam-search3 (zip 
                                                   (list initial-prog)
                                                   (score-programs (list initial-prog)))
                                                 (list initial-prog (car (score-programs (list initial-prog))))
                                                 beam-size ;; (if (not (null? stop-at-depth)) (car stop-at-depth) 0)
                                                 program->transforms
                                                 score-programs
                                                 fringe->merged-fringe
                                                 ;; (lambda (progs) progs)
                                                 (if (not (null? stop-at-depth)) depth-stop (same-prog-stop 20)))])
             learned-program))
         )
