(library (program-likelihood)
         (export log-prob-sum
                 parse-tree->prob
                 parse-tree->log-prob
                 parse-dag->log-prob

                 data-program->log-likelihood
                 data-program->log-posterior

                 data-program->posterior

                 no-choices?
                 batch-data-program->posterior)
         (import (except (rnrs) string-hash string-ci-hash)
                 (_srfi :1)
                 (_srfi :69)
                 (chart-parsing)
                 (util)
                 (sym)
                 (printing)
                 (program))

         (define (parse-tree->prob tree)
           (cond [(eq? 'tree (car tree)) (let* ([subtrees (cddddr tree)]
                                                [my-prob (/ 1 (cadddr tree))])
                                           (* my-prob 
                                              (apply * (map parse-tree->prob subtrees))))]
                 [(list? (car tree)) (apply + (map parse-tree->prob tree))]
                 [else 1]))

         (define (log-prob-sum . xs)
           (define (bin-log-prob x y)
             (+ y (log (+ 1 (exp (- x y))))))
           (fold bin-log-prob (car xs) (cdr xs)))

         (define (parse-tree->log-prob tree)
           (cond [(null? tree) -inf.0]
                 [(eq? 'parse_node (car tree)) (let* ([subtrees (cddddr tree)]
                                                      [my-prob (log (/ 1 (cadddr tree)))])
                                                 (+ my-prob 
                                                    (apply + (map parse-tree->log-prob subtrees))))]
                 [(list? (car tree)) (apply log-prob-sum (map parse-tree->prob tree))]
                 [else 1]))
         ;; Calculating inside probability of entire dag
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
           (define node->lhs-sym cadr)
           (define node->rule-id caddr)
           (define node->num-rules cadddr)
           (define node->children-ids cddddr)

           (define dag->nodes cadr)
           (define dag->roots car)

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
             (parse-dag->log-prob (run-chart-parse (program->scfg prog) data))))

         (define (data-program->likelihood data prog)
           (if (no-choices? prog) 1.0
             (parse-tree->prob (run-chart-parse (program->scfg prog) data))))

         (define (program->prior prog)
           (- (program-size prog)))

         (define (data-program->log-posterior data prog . weights)
           (let* ([lp-weights (cond [(null? weights) '(1.0 1.0)]
                                    [else weights])]
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

         (define (batch-data-program->posterior data progs . likelihood-prior-weights)
           (define (iterator charts 
                             programs 
                             scores)
             (cond [(null? programs) (reverse scores)]

                   [else (let* ([likelihood-weight (cond [(null? likelihood-prior-weights) 1.0]
                                                         [else (car likelihood-prior-weights)])]
                                [prior-weight (cond [(null? likelihood-prior-weights) 1.0]
                                                    [else (cadr likelihood-prior-weights)])]
                                [prior (* prior-weight (program->prior (car programs)))]
                                ;; [db (begin (pretty-print (list likelihood-weight prior-weight prior)))]

                                )

                           (cond [(no-choices? (car programs)) (iterator charts 
                                                                         (cdr programs) 
                                                                         (cons prior scores))]
                                 [else 

                                   (let* ([likelihood 
                                            (* likelihood-weight
                                               (apply + 
                                                      ;; To be replaced with 
                                                      ;; (expectation-maximized-log-probs (car charts))
                                                      ;; [ParseTree] -> [Float]
                                                      (map parse-dag->log-prob (car charts))
                                                      ))])

                                     (iterator (cdr charts) 
                                               (cdr programs) 
                                               (cons (+ likelihood 
                                                        prior) scores)))]))]))

           (let* ([progs-with-choices (filter (lambda (p) (not (no-choices? p))) progs)]
                  [all-charts (if (null? progs-with-choices) '() 
                                (batch-run-chart-parse (map program->scfg progs-with-choices) data))]
                  [scores (iterator all-charts progs '())])
             (begin ;; (print "batch scores: ~s" scores)
               scores)))

         )

