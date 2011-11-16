(library (parameter-estimation)
        (export 
            train-parameters
            )
        (import (except (rnrs) string-hash string-ci-hash)
            (_srfi :1)
            (_srfi :69)
            (printing)
            (util)
            (program)
            (named-search-trees)
            (hash-cons))

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

;; UPDATE PARAMETERS
(define (grammar->update-params dags)

;; HASH TABLE DEFS
;; ids -> counts
(define counts-table (make-hash-table equal?))
;; ids -> NT counts
(define NT-counts-table (make-hash-table equal?))

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
               [my-prob (rule-param node)]
               [children-ids (node->children-ids node)] 
               [answer
                 (+ my-prob
                    (apply +
                           (map (lambda (desc) 
                                  (apply log-prob-sum
                                         (map (lambda (id) 
                                                (in-log-prob id))
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
        [new-value (apply log-prob-sum (cons (- (+ (out-log-prob node-id) (in-log-prob node-id)) (+ example-prob (rule-param (id->def node-id)))) (flatten (list curr-value))))])
        (begin (hash-table-set! exp-counts-table key new-value) new-value)))

  (define (exp-counts node-id)
    (let* ([key (rule-key (id->def node-id))])
    (hash-table-ref exp-counts-table key)))

  (begin 
         (for-each ref-node (dag->nodes dag))
         (map compute-exp-counts (map car (dag->nodes dag)))
         exp-counts-table))

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
    )
)

(define (grammar->log-likelihood dags)
    (define (log-likelihood dag)
        
        ;; HASH TABLE DEFS
        ;; ids -> defs
        (define node-table (make-hash-table equal?))
        ;; ids -> inner probs
        (define in-prob-table (make-hash-table equal?))

        ;; INITS NODE-TABLE
        (define (ref-node node)
            (let* ([id (car node)]
                [def (cadr node)])
            (hash-table-ref node-table id (lambda () (begin (hash-table-set! node-table id def) def)))))

        (define (id->def id) (hash-table-ref node-table id))

        ;; INITS INSIDE-PROB TABLE
        (define (in-log-prob node-id)
            (hash-table-ref 
            in-prob-table 
            node-id 
            (lambda () 
                (let* ([node (id->def node-id)]
                    [my-prob (rule-param node)]
                    [children-ids (node->children-ids node)] 
                    [answer
                        (+ my-prob
                            (apply +
                                (map (lambda (desc) 
                                    (apply log-prob-sum 
                                        (map (lambda (id) 
                                            (in-log-prob id))
                                        desc)))
                                children-ids)))])
                (begin (hash-table-set! in-prob-table node-id answer) answer)))))
        
        (begin 
            (for-each ref-node (dag->nodes dag))
            (apply log-prob-sum (map in-log-prob (dag->roots dag)))
        )
    )

    (begin
        (apply + (map log-likelihood dags))
    )
)

(define (io-iter n last-likelihood output-dags)
    (cond [(= 0 n) rule-param-table]
        [else
            (begin
                (let* ([log-likelihood (grammar->log-likelihood output-dags)])
                    (begin
                        ;;(pretty-print (exp log-likelihood))
                        (cond [(= log-likelihood last-likelihood) rule-param-table]
                            [else
                                (grammar->update-params output-dags)
                                ;;(pretty-print (map exp (map cdr (hash-table->alist rule-param-table))))
                                (io-iter (- n 1) log-likelihood output-dags)])
                    ))
            )]))

(define (train-parameters dags)
  (begin
    (set! rule-param-table (make-hash-table equal?))
    (let* ([trained-params (hash-table->alist (io-iter 1000 0 dags))]
           [final-likelihood (grammar->log-likelihood dags)])
      (list final-likelihood trained-params))))
)
