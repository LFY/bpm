(library (lsys-induction)
         (export 

           data->lsys-data
           successor-replace
           grammar->lsys-parse-form
           remap-dag-idxs)

         (import (except (rnrs) string-hash string-ci-hash)
                 (program)
                 (grammars)
                 (chart-parsing)
                 (grammar-induction)
                 (parameter-estimation)
                 (util)
                 (_srfi :1)
                 (_srfi :69)
                 (delimcc-simple-ikarus)
                 (grammar-derivations-spread)
                 )

         (define stop-sym 'stop)

         (define (data->lsys-data data)
           (subexpr-walk
             (lambda (t) (if (and (equal? 'elem (car t))
                              (null? (cddr t)))
                           `(elem ,(cadr t) stop)
                           t))
             data))

         (define (has-succ? choice)
           (reset
             (begin
               (subexpr-walk
                 (lambda (t)
                   (if (call? t)
                     (shift k #t) t))
                 choice)
               #f)))
                           
             
         (define (call? x)
           (and (= 1 (length x))
                (symbol? (car x))
                (equal? "F" (substring (symbol->string (car x)) 0 1))))

         (define (successor-replace choice)
             (cond [(has-succ? choice)
                    (subexpr-walk
                      (lambda (t)
                        (if (equal? 'tr (car t)) stop-sym t))
                      choice)]
                   [else `(elem ,(cadr choice) ,stop-sym)]))

         (define (grammar->lsys-parse-form gr)
           (define (nt->lsys-parse-form nt)
             (let* ([choices (nt->choices nt)])
               `(abstraction 
                  ,(abstraction->name nt) ()
                  (choose ,@(append (map (lambda (c) (cond [(has-succ? c) c]
                                                           [else (successor-replace c)])) choices)
                                    (map successor-replace (filter has-succ? choices)))))))
           (let* ([nts (program->abstractions gr)]
                  [revised-nts (map nt->lsys-parse-form nts)])
             (grammar-with-new-nts+body gr revised-nts (program->body gr))))

         ;; principle:
         ;; use same order of NTs as original grammar
         ;; for each nt:
         ;; get filtered version of nts: choices w/ successors, in same order
         ;; start counting from (length choices), these are the indices we want to remap
         (define (remap-dag-idxs dag original-grammar)
           (define node->lhs-sym cadr)
           (define node->rule-id caddr)
           (define node->num-rules cadddr)
           (define node->children-ids cddddr)
           (define dag->nodes cadr)
           (define dag->roots car)
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

           (define lsys-dag-idx->original-idx (make-hash-table equal?))
           (define (rename-rule-id node)
             (let* ([lhs-sym (remove-prefix (node->lhs-sym node))]
                    [original-nt (lookup-original-nt lhs-sym)]
                    [
           `(,(dag->roots dag)
              ,(map 
                 (lambda (idx-node)
                   (rename-rule-id (cadr idx-node)))
                 (dag->nodes dag))))

         


         )

