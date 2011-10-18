(library (grammar-induction)
         (export gi-bmm)

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

         (define (nt->choices nt)
           (define (choice? body) (eq? 'choose (car body)))
           (let* ([main-body (abstraction->pattern nt)])
             (cond [(choice? main-body) (cdr main-body)]
                   [else (list main-body)])))

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

         (define-timed 
           (gi-bmm data beam-size likelihood-weight prior-weight . stop-at-depth)

           ;; TODO: Which one is right?
           (define prog-table (make-hash-table equal?))

           ;; (define-timed (program->exists? prog likelihood)
           ;;               (define (prog->unlabeled prog)
           ;;                 (define (abstr->num-successors prog abstr)
           ;;                   (let* ([is-successor? (lambda (expr)
           ;;                                           (and (non-empty-list? expr)
           ;;                                                (= 1 (length expr))
           ;;                                                (contains? (car expr) (map abstraction->name (program->abstractions prog)))))])
           ;;                     (length (sexp-search is-successor? (lambda (x) x) (abstraction->pattern abstr)))))
           ;;                 (map (curry abstr->num-successors prog) (program->abstractions prog)))
           ;;               (let* ([hash (list likelihood (prog->unlabeled prog))])
           ;;                 (if (hash-table-exists? prog-table hash) #t
           ;;                   (begin (hash-table-set! prog-table hash prog) #f))))

           ;; (define-timed (fringe->merged-fringe prog-likelihoods) ;; can result in starvation of the beam
           ;;               (define (iterator prog-likelihoods new-fringe)
           ;;                 (cond [(null? prog-likelihoods) new-fringe]
           ;;                       [else 
           ;;                         (let* ([prog-likelihood (car prog-likelihoods)]
           ;;                                [prog (car prog-likelihood)]
           ;;                                [likelihood (cadr prog-likelihood)])
           ;;                           (begin (if (program->exists? prog likelihood)
           ;;                                    (iterator (cdr prog-likelihoods) new-fringe)
           ;;                                    (iterator (cdr prog-likelihoods) (cons prog-likelihood new-fringe)))))]))
           ;;               (iterator prog-likelihoods '()))

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
                    ;; [db (print likelihood)]
                    )
               (begin 
                 ;;(pretty-print answer)
                      answer)))
           ;; (list likelihood)))


           (define-timed (fringe->merged-fringe prog-likelihoods) ;; can result in starvation of the beam
                         ;; (delete-duplicates prog-likelihoods ;; this is quadratic time, should be n log n
                         ;;                    (lambda (x y) 
                         ;;                      (equal? (prog-likelihood->hash x)
                         ;;                              (prog-likelihood->hash y)))))
                         (delete-duplicates-by-hash prog-likelihood->hash prog-likelihoods))

           ;; better version of delete-duplicates: takes a hash function specifying which elements are "equal," this is n log n time instead of quadratic like delete-duplicates.

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



           (define-timed (same-prog-stop limit)
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

           (define-timed (score-programs progs)
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


         )
