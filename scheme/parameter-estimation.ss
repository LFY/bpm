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
                            (er (my5 (er (my5 (start)))))
                            (er)
                            (er (my5 (er)))
                            ))

  (
   )))

(define output-dag
  (run-chart-parse test-scfg2
		   '(er (my5 (er (my5 (er (my5 (er)))))))
                   ))

(define another-dag
(run-chart-parse test-scfg2
'(er (my5 (er (my5 (er)))))
))

(define output-dags (list output-dag another-dag))

(pretty-print output-dags)

(define (flatten list)
(cond ((null? list) '())
((list? (car list)) (append (flatten (car list)) (flatten (cdr list))))
(else
(cons (car list) (flatten (cdr list))))))

;; ACCESSORS
(define node->lhs-sym cadr)
(define node->rule-id caddr)
(define node->num-rules cadddr)
(define node->children-ids cddddr)
(define dag->nodes cadr)
(define dag->roots car)

;; HASH TABLE DEFS
;; rules -> params
(define rule-param-table (make-hash-table equal?))

;; INITS RULE-PARAM TABLE
(define (rule-param node)
(let* ([key (rule-key node)])
(hash-table-ref rule-param-table key (lambda () (let* ([param (log (/ 1.0 (node->num-rules node)))]) (begin (hash-table-set! rule-param-table key param) param))))))

(define (rule-key node) (cons (node->lhs-sym node) (cons (node->rule-id node) (cons (node->num-rules node) '()))))

;; GRAMMAR LOG-LIKELIHOOD
(define (grammar->log-likelihood dags)
;; HASH TABLE DEFS
;; dag -> log-likelihood
(define log-likelihood-table (make-hash-table equal?))

;; EXPECTED COUNTS FOR DAG NODES
(define (parse-dag->exp-counts dag)
  
  ;; HASH TABLE DEFS
  ;; ids -> defs
  (define node-table (make-hash-table equal?))
  ;; ids -> parent list
  (define parents-table (make-hash-table equal?))
  ;; (id parent-id)s -> list of sibling lists
  (define siblings-table (make-hash-table equal?))
  ;; ids -> inner probs
  (define in-prob-table (make-hash-table equal?))
  ;; ids -> outside probs
  (define out-prob-table (make-hash-table equal?))
  ;; ids -> expected counts
  (define exp-counts-table (make-hash-table equal?))

  ;; INITS NODE-TABLE
  (define (ref-node node)
    (let* ([id (car node)]
      [def (cadr node)])
    (hash-table-ref node-table id (lambda () (begin (hash-table-set! node-table id def)
    def)))))

  (define (id->def id) (hash-table-ref node-table id))

  ;; INITS PARENTS-TABLE
  (define (node->parents node-id)
  (hash-table-ref parents-table node-id
  (lambda () (let* ([parent-list (flatten (map (add-parent node-id) (dag->nodes dag)))])
  (begin (hash-table-set! parents-table node-id parent-list) parent-list)))))

  ;; how to return nothing in scheme?
  (define (add-parent node-id) 
  (lambda (parent)
  (let* ([parent-id (car parent)] [parent-def (ref-node parent)] [child-ids (node->children-ids parent-def)])
  (if (member node-id (flatten child-ids)) parent-id '()))))

  ;; INITS SIBLING-TABLE
  (define (node->siblings node-id parent-id)
  (hash-table-ref siblings-table (list node-id parent-id)
  (lambda () (let* ([siblings-list (create-sibling-list node-id (node->children-ids (id->def parent-id)) '())])
  (begin (hash-table-set! siblings-table (list node-id parent-id) siblings-list) siblings-list)))))

  (define (create-sibling-list node-id child-list sibling-list) 
  (if (null? child-list) sibling-list 
  (create-sibling-list node-id (cdr child-list) 
  (if (member node-id (car child-list)) sibling-list (cons (car child-list) sibling-list)))))

  ;; INITS INSIDE-PROB TABLE
  (define (in-log-prob node-id)
    (hash-table-ref 
      in-prob-table 
      node-id 
      (lambda () 
        (let* ([node (id->def node-id)]

               ;; Uniform probability ; to be replaced with something that references a table of rules->parameters
               [my-prob (rule-param node)]

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
                                                (in-log-prob id)) ;; inside_probability(<descendant-i-parse-j>)
                                              desc)))
                                children-ids)))])
          (begin (hash-table-set! in-prob-table node-id answer)
                 answer)))))


  ;; INITS OUTSIDE-PROB TABLE
  (define (out-log-prob node-id)
    (hash-table-ref
     out-prob-table
     node-id
     (lambda ()
       (if (member node-id (dag->roots dag)) 0 
         (let* ([answer (apply log-prob-sum (map (out-log-prob-helper node-id) (node->parents node-id)))])
           (begin (hash-table-set! out-prob-table node-id answer)
             answer))))))

  (define (out-log-prob-helper node-id)
  (lambda (parent-id)
    (let* ([siblings-list (node->siblings node-id parent-id)])
    (+ (rule-param (id->def node-id))
    (+ (out-log-prob parent-id)
    (apply + 
      (map (lambda (desc)
        (apply log-prob-sum
          (map (lambda (id)
            (in-log-prob id))
              desc)))
         siblings-list)))))))

  ;; EXPECTED COUNTS TABLE
  (define (compute-exp-counts node-id)
     (let* (
        [key (rule-key (id->def node-id))]
        [curr-value (hash-table-ref exp-counts-table key
                    (lambda () '()))]
        [example-prob (apply log-prob-sum (map in-log-prob (dag->roots dag)))]
        [new-value (apply log-prob-sum (cons (- (+ (out-log-prob node-id) (in-log-prob node-id)) example-prob) (flatten (list curr-value))))])
        (begin (hash-table-set! exp-counts-table key new-value) new-value)))

  (define (exp-counts node-id)
    (let* ([key (rule-key (id->def node-id))])
    (hash-table-ref exp-counts-table key)))

  (begin 
         (for-each ref-node (dag->nodes dag))
         (map compute-exp-counts (map car (dag->nodes dag)))
         (let* (
            [value (apply log-prob-sum (map in-log-prob (dag->roots dag)))]
            [curr-value (hash-table-ref log-likelihood-table dag (lambda () '()))]
            [new-value (apply log-prob-sum value (flatten (list curr-value)))])
         (hash-table-set! log-likelihood-table dag new-value))
         exp-counts-table))

(define (grammar->update-params dags)

  ;; ids -> counts
  (define counts-table (make-hash-table equal?))
  ;; ids -> NT counts
  (define NT-counts-table (make-hash-table equal?))

  ;; COUNTS TABLE
  (define (compute-counts entry)
    (let* (
      [key (car entry)]
      [value (cdr entry)]
      [curr-value (hash-table-ref counts-table key (lambda () '()))]
      [new-value (apply log-prob-sum value (flatten (list curr-value)))])
    (begin (hash-table-set! counts-table key new-value) new-value)))

  ;; NT COUNTS TABLE
  (define (compute-NT-counts entry)
    (let* (
      [key (caar entry)]
      [value (cdr entry)]
      [curr-value (hash-table-ref NT-counts-table key (lambda () '()))]
      [new-value (apply log-prob-sum value (flatten (list curr-value)))])
    (begin (hash-table-set! NT-counts-table key new-value) new-value)))

  ;; UPDATE PARAMS
  (define (update-params entry)
    (let* (
      [key1 (car entry)]
      [key2 (caar entry)]
      [numerator (hash-table-ref counts-table key1)]
      [denominator (hash-table-ref NT-counts-table key2)])
    (hash-table-set! rule-param-table key1 (- numerator denominator))))

  (begin
    (let* ([exp-counts (flatten (map hash-table->alist (map parse-dag->exp-counts dags)))])
    (map compute-counts exp-counts)
    (map compute-NT-counts exp-counts))
    (map update-params (hash-table->alist counts-table))
    (apply + (map exp (map cdr (hash-table->alist rule-param-table))))
    ))

  (begin
    (pretty-print (grammar->update-params dags))
    (exp (apply log-prob-sum (map cdr (hash-table->alist log-likelihood-table))))
  )
)
;; lexigraphic ordering
;; what is the log-likelihood? inside(start)?
;; don't we care about the params to generate samples; where do I set these params?

(begin
(pretty-print (grammar->log-likelihood output-dags))
(pretty-print (hash-table->alist rule-param-table))
(pretty-print (map exp (map cdr (hash-table->alist rule-param-table))))
(pretty-print "")
(pretty-print (grammar->log-likelihood output-dags))
(pretty-print (map exp (map cdr (hash-table->alist rule-param-table))))
(pretty-print "")
(pretty-print (grammar->log-likelihood output-dags))
(pretty-print (map exp (map cdr (hash-table->alist rule-param-table))))
(pretty-print "")
(pretty-print (grammar->log-likelihood output-dags))
(pretty-print (map exp (map cdr (hash-table->alist rule-param-table))))
(pretty-print "")
(pretty-print (grammar->log-likelihood output-dags))
(pretty-print (map exp (map cdr (hash-table->alist rule-param-table))))
(pretty-print "")
(pretty-print (grammar->log-likelihood output-dags))
(pretty-print (map exp (map cdr (hash-table->alist rule-param-table))))
(pretty-print "")
(pretty-print (grammar->log-likelihood output-dags))
(pretty-print (map exp (map cdr (hash-table->alist rule-param-table))))
(pretty-print "")
(pretty-print (grammar->log-likelihood output-dags))
(pretty-print (map exp (map cdr (hash-table->alist rule-param-table))))
(pretty-print "")
(pretty-print (grammar->log-likelihood output-dags))
(pretty-print (map exp (map cdr (hash-table->alist rule-param-table))))
(pretty-print "")
(pretty-print (grammar->log-likelihood output-dags))
(pretty-print (map exp (map cdr (hash-table->alist rule-param-table))))
(pretty-print "")
)