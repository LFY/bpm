(import 
        (_srfi :1)
        (_srfi :69)
        (printing)
        (util)
        (chart-parsing)
        (program)
        (named-search-trees)
        (program-likelihood)
        (hash-cons))

(define test-scfg2
  '(
    (define (start) (choose (er (my5 (start)))
                            (er (my5 (er)))
                            (er (my5 (er (my5 (er)))))
                            (er (my5 (er (my5 (Start)))))
                            ))

  (
   )))

(define output-dag
  (run-chart-parse test-scfg2
                   '(er (my5 (er (my5 (er (my5 (er (my5 (er (my5 (er (my5 (er (my5 (er)))))))))))))))
                   
                   ))

(pretty-print output-dag)

;; DAG format:
;; (<list of root IDs> <list of nodes>)
;; each node:
;; (<ID> (parse_node <LHS symbol> <rule ID> <# total rules> 
;;                   <list of descendant IDs for successor 1> 
;;                   <for successor 2> ...))

;; Example:
'((12 13)
  ((1 (parse_node sym_Start 1 4))
    (2 (parse_node sym_Start 0 4 (1)))
    (3 (parse_node sym_Start 2 4))
    (4 (parse_node sym_Start 0 4 (2 3)))
    (5 (parse_node sym_Start 3 4 (1)))
    (6 (parse_node sym_Start 0 4 (4 5)))
    (7 (parse_node sym_Start 3 4 (2 3)))
    (8 (parse_node sym_Start 0 4 (6 7)))
    (9 (parse_node sym_Start 3 4 (4 5)))
    (10 (parse_node sym_Start 0 4 (8 9)))
    (11 (parse_node sym_Start 3 4 (6 7)))
    (12 (parse_node sym_Start 0 4 (10 11)))
    (13 (parse_node sym_Start 3 4 (8 9)))))

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

(print (parse-dag->log-prob output-dag))

