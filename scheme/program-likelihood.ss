(library (program-likelihood)
         (export log-prob-sum
                 log-prob-sum2
                 parse-tree->prob
                 parse-tree->log-prob
                 parse-dag->log-prob
                 parse-dag+features->log-prob

                 norm-pdf

                 exec-chart->log-prob

                 data-program->log-likelihood
                 data-program->log-posterior

                 data-program->posterior

                 no-choices?
                 batch-data-program->posterior
                 batch-data-program->sum-posterior

                 reformat-exec-chart
                 )
         (import 
           (except (rnrs) string-hash string-ci-hash)
                 (_srfi :1) ;; lists
                 (_srfi :69) ;; hash tables
                 (util) ;; various utility functions
                 (printing) ;; printing 
                 (chart-parsing)
                 (sym)
                 (program))

         (define (parse-tree->prob tree)
           (cond [(eq? 'tree (car tree)) (let* ([subtrees (cddddr tree)]
                                                [my-prob (/ 1 (cadddr tree))])
                                           (* my-prob 
                                              (apply * (map parse-tree->prob subtrees))))]
                 [(list? (car tree)) (apply + (map parse-tree->prob tree))]
                 [else 1]))


         (define (parse-tree->log-prob tree)
           (cond [(null? tree) -inf.0]
                 [(eq? 'parse_node (car tree)) (let* ([subtrees (cddddr tree)]
                                                      [my-prob (log (/ 1 (cadddr tree)))])
                                                 (+ my-prob 
                                                    (apply + (map parse-tree->log-prob subtrees))))]
                 [(list? (car tree)) (apply log-prob-sum (map parse-tree->prob tree))]
                 [else 1]))
         ;; Calculating inside probability of entire dag
         ;; The main acccessors
         
         (define dag->nodes cadr)
         (define dag->roots car)

         (define features->nodes cadr)

         (define node->lhs-sym cadr)
         (define node->rule-id caddr)
         (define node->num-rules cadddr)
         (define node->children-ids cddddr)

         (define pi 3.141592653589793236)

         (define (norm-pdf mean var smp)
  (*
    (/ 1.0 (expt (* (* 2 pi) var)
                 0.5))
    (exp (- (* (* (/ 1.0 2.0) (expt (- smp mean) 2.0)) 
               (/ 1.0 (* 2.0 var)))))))

         (define (parse-dag->log-prob dag)

           ;; We use two hash tables to do a proper traversal
           ;; mapping ids to their node definitions
           (define node-table (make-hash-table equal?))
           ;; mapping ids to their probabilities
           (define prob-table (make-hash-table equal?))

           (define (ref-node node)
             (let* ([id (car node)]
                    [def (cadr node)])
               (hash-table-ref node-table id (lambda () (begin (hash-table-set! node-table id def)
                                                               def)))))

           (define (id->def id) (hash-table-ref node-table id))

           ;; accessors (compare with example structure)

           ;; Traversing the DAG to compute inside probability
           ;; We may want a tail-recursive version if even the DAGs get too large.
           (define (compute-log-prob node-id)

             ;; This interleaving of hash-table-ref is what makes us truly follow dag
             ;; structure; despite being a tree-recursive function, compute-log-prob
             ;; only computes as many probabilities as there are descendants of node-id.
             ;; The semantics of hash-table-ref: if the id cannot be found, execute the
             ;; 3rd argument

             (hash-table-ref 
               prob-table 
               node-id 
               (lambda () 
                 (let* ([node (id->def node-id)]

                        ;; Uniform probability ; to be replaced with something that references a table of rules->parameters
                        [my-prob (log (/ 1 (node->num-rules node)))]

                        ;; Children of node, as a list of list of children-ids representing alternative parses.
                        ;; There may be more than one list of children-ids, for rules with more than one successor.
                        [children-ids (node->children-ids node)] 

                        ;; The sum-product
                        [answer ;; inside_probability(<LHS-of-my-rule>) = 
                          (+ my-prob ;; \theta(<my-rule>) * 
                             (apply + ;; \prod^|#children-ids|_{i = 1} 
                                    (map (lambda (desc) 
                                           (apply log-prob-sum ;; \sum^|#alternative-parses-of-child-i|_{j = 1}
                                                  (map (lambda (id) 
                                                         (compute-log-prob id)) ;; inside_probability(<descendant-i-parse-j>)
                                                       desc)))
                                         children-ids)))])
                   (begin (hash-table-set! prob-table node-id answer)
                          answer)))))

           (begin (for-each ref-node (dag->nodes dag))
                  ;; Top-level sum for the roots
                  (apply log-prob-sum (map compute-log-prob (dag->roots dag)))))

         (define (parse-dag+features->log-prob dag+features)

           ;; We use two hash tables to do a proper traversal
           ;; mapping ids to their node definitions
           (define node-table (make-hash-table equal?))
           ;; mapping ids to their probabilities
           (define prob-table (make-hash-table equal?))

           (define feature-table (make-hash-table equal?))

           (define (ref-feature feature-node)
             (let* ([id (car feature-node)]
                    [def (cadr feature-node)])
               (hash-table-ref
                 feature-table
                 id
                 (lambda () (begin (hash-table-set! feature-table id def)
                                   def)))))

           (define (ref-node node)
             (let* ([id (car node)]
                    [def (cadr node)])
               (hash-table-ref node-table id (lambda () (begin (hash-table-set! node-table id def)
                                                               def)))))

           (define (id->def id) (hash-table-ref node-table id))

           (define (compute-feature node-id)

             (let* ([def (hash-table-ref feature-table node-id
                                         (lambda () '()))]
                    ;; [db (print "FEATURE DEF: ~s" def)]
                    )
               (cond [(null? def) 0.0]
                     [(contains? (car def) '(gauss gaussian))
                      (let* ([mean (cadr def)]
                             [var (caddr def)]
                             [smp (cadddr def)])

                        (begin ;; (print def)
                               ;; (print "mean: ~s var: ~s smp: ~s" mean var smp)
                               ;; (print (log (norm-pdf mean var smp)))
                               (log (norm-pdf mean var smp))))]
                     [else 0.0])))
                      

           ;; Traversing the DAG to compute inside probability
           ;; We may want a tail-recursive version if even the DAGs get too large.
           (define (compute-log-prob node-id)

             ;; This interleaving of hash-table-ref is what makes us truly follow dag
             ;; structure; despite being a tree-recursive function, compute-log-prob
             ;; only computes as many probabilities as there are descendants of node-id.
             ;; The semantics of hash-table-ref: if the id cannot be found, execute the
             ;; 3rd argument

             (hash-table-ref 
               prob-table 
               node-id 
               (lambda () 
                 (let* ([node (id->def node-id)]

                        ;; Uniform probability ; to be replaced with something that references a table of rules->parameters
                        [my-prob 
                          (+ 
                               ;; (print "COMPUTE FEATURE") 
                               (compute-feature node-id)
                             (log (/ 1 (node->num-rules node))))]
                        

                        ;; Children of node, as a list of list of children-ids representing alternative parses.
                        ;; There may be more than one list of children-ids, for rules with more than one successor.
                        [children-ids (node->children-ids node)] 

                        ;; The sum-product
                        [answer ;; inside_probability(<LHS-of-my-rule>) = 
                          (+ my-prob ;; \theta(<my-rule>) * 
                             (apply + ;; \prod^|#children-ids|_{i = 1} 
                                    (map (lambda (desc) 
                                           (apply log-prob-sum ;; \sum^|#alternative-parses-of-child-i|_{j = 1}
                                                  (map (lambda (id) 
                                                         (compute-log-prob id)) ;; inside_probability(<descendant-i-parse-j>)
                                                       desc)))
                                         children-ids)))])
                   (begin (hash-table-set! prob-table node-id answer)
                          answer)))))

           (let* ([dag (reformat-exec-chart (car dag+features))]
                  [features (cadr dag+features)]
                                    )
             (begin (for-each ref-node (dag->nodes dag))
                    (for-each ref-feature (features->nodes features))
                    ;; Top-level sum for the roots
                    (apply log-prob-sum (map compute-log-prob (dag->roots dag))))))


         (define (reformat-exec-chart chart)
           (let* ([nodes (dag->nodes chart)]
                  [new-nodes (map (lambda (def)
                                    (let ([node (cadr def)])
                                      `(,(car def) 
                                         ,`(tree ,(cadr node)
                                                 ,(caddr node)
                                                 ,(cadddr node)
                                                 ,@(cdr (cddddr node))))))
                                  nodes)])
             `(,(dag->roots chart) ,new-nodes)))

         (define (exec-chart->log-prob exec-chart)
           (cond [(null? exec-chart) -inf.0]
                 [else (let* ([old-dag-format (reformat-exec-chart exec-chart)])
                         (parse-dag->log-prob old-dag-format))]))


         ;; Input: a list of charts, of the form

         ;; data Tree = Tree LHSSymbol SuccessorID NumSuccessors Chart1 Chart2 ...
         ;; data Chart = Chart [Tree]

         ;; output: the log probability of all charts, computed with parameters
         ;; determined through EM

         (define (expectation-maximized-log-probs trees) '())

         (define (no-choices? prog)
           (let* (;; [condensed-program (condense-program prog)]
                  [choices (deep-find-all (lambda (x) (cond [(list? x) (cond [(eq? 'choose (car x)) (> (length (cdr x)) 1)]
                                                                             [else #f])]
                                                            [else #f])) prog)])
             (null? choices)))

         (define (data-program->log-likelihood data prog)
           (if (no-choices? prog) 0.0
             (let* ([chart-parse-result (run-chart-parse (program->scfg prog) data)])
               (cond [(null? chart-parse-result)
                      -inf.0]
                     [else (parse-dag->log-prob chart-parse-result)]))))

         (define (data-program->likelihood data prog)
           (if (no-choices? prog) 1.0
             (parse-tree->prob (run-chart-parse (program->scfg prog) data))))

         ;; (define (program->prior prog)
         ;;   (begin (print "in program->prior: program size: ~s" (program-size prog))
         ;;          (- (program-size prog))))
         (define (program->prior prog)
           ;; (begin (print "in program->prior: grammar size: ~s" (grammar-size prog))
                  (- (grammar-size prog)))

         (define (grammar-size prog)
           (+ (apply + (map (lambda (abstr) (+ 1  ;; + 1: The "separator" symbol between nonterminals basically encourages merging
                                               (cond [(eq? 'choose (car (abstraction->pattern abstr))) ;; Choose operator does not count, so subtract 1 for using choose
                                                      (- (sexpr-size (abstraction->pattern abstr)) 1)]
                                                       [else (sexpr-size (abstraction->pattern abstr))])))
                            (program->abstractions prog)))
              (sexpr-size (program->body prog))))

         (define (data-program->log-posterior data prog . params)
           (let* ([lp-weights (cond [(null? params) '(1.0 1.0)]
                                    [else params])]
                  [likelihood-weight (car lp-weights)]
                  [prior-weight (cadr lp-weights)]
                  [likelihood (* likelihood-weight 
                                 (data-program->log-likelihood data prog)) ]
                  [prior (* prior-weight (program->prior prog))])
             (begin ;; (print "Likelihood: ~s" likelihood)
               ;; (print "Prior: ~s" prior)
               (+ likelihood prior))))


         (define (data-program->posterior data prog)
           (* (data-program->likelihood data prog) (exp (program->prior prog))))

         ;; params:
         ;; likelihood-weight prior-weight use-features
         (define (batch-data-program->posterior data progs . params)

           (define use-features? (= 3 (length params)))

           (define (iterator charts 
                             programs 
                             scores)
             (cond [(null? programs) (reverse scores)]

                   [else (let* ([likelihood-weight (cond [(null? params) 1.0]
                                                         [else (car params)])]
                                [prior-weight (cond [(null? params) 1.0]
                                                    [else (cadr params)])]
                                [prior (* prior-weight (program->prior (car programs)))]
                                ;; [db (begin (pretty-print (list likelihood-weight prior-weight prior)))]

                                )

                           (cond [(no-choices? (car programs)) (iterator charts 
                                                                         (cdr programs) 
                                                                         (cons prior scores))]
                                 [else 

                                   (let* ([inside-prob-fx (cond [use-features? parse-dag+features->log-prob]
                                                                [else exec-chart->log-prob])]
                                          [likelihood 
                                            (* likelihood-weight
                                               (apply 
                                                 + ;; product of the exemplar probabilities
                                                 ;; log-prob-sum2  ;; sum of the exemplar probabilities; to deal with noise.
                                                      ;; To be replaced with 
                                                      ;; (expectation-maximized-log-probs (car charts))
                                                      ;; [ParseTree] -> [Float]
                                                      ;; (map parse-dag->log-prob (car charts))
                                                      (map inside-prob-fx (car charts))
                                                      ))])

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


         (define (batch-data-program->sum-posterior data progs . params)

           (define use-features? (= 3 (length params)))

           (define (iterator charts 
                             programs 
                             scores)
             (cond [(null? programs) (reverse scores)]

                   [else (let* ([likelihood-weight (cond [(null? params) 1.0]
                                                         [else (car params)])]
                                [prior-weight (cond [(null? params) 1.0]
                                                    [else (cadr params)])]
                                [prior (* prior-weight (program->prior (car programs)))]
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
                                                 ;; + ;; product of the exemplar probabilities
                                                 log-prob-sum2
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

         ;; in the bpm report, the 'data' actually includes sampling functions like gaussians.
         ;; each data point then represents a potential population of actual data points.
         ;; bpm is actually done over the population; 
         
         ;; we generate a population of samples (at the beginning) and compute
         ;; the parameter score by matching against these samples and using the
         ;; support for feature functions.


         )

