(library (chart-parsing)
         (export program->scfg
                 scfg->pl
                 run-chart-parse)
         (import (except (rnrs) string-hash string-ci-hash)
                 (rnrs eval)
                 (program)
                 (sym)
                 (util)
                 (printing)
                 (_srfi :1)
                 (_srfi :69)

                 (named-search-trees)
                 (prolog-serialize)
                 (scfg)
                 (only (scheme-tools) system)
                 )

         (define (nondet-desugar-body body)
           `(define-nondet (Start) ,(caddr body)))

         (define (nondet-desugar-abstraction abstr)
           `(define-nondet ,(cons (abstraction->name abstr)
                                  (abstraction->vars abstr))
                           ,(abstraction->pattern abstr)))

         (define (nondet-desugar-abstractions prog)
           (map nondet-desugar-abstraction (program->abstractions prog)))

         (define (program->scfg prog) 
           (let* ([desugared-body (nondet-desugar-body (program->body prog))]
                  [desugared-abstractions (nondet-desugar-abstractions prog)])
             (eval 
               `((lambda ()
                   ,@desugared-abstractions 
                   ,desugared-body
                   (nondet-program->named-search-tree Start)))
               (environment '(rnrs) '(named-search-trees) '(node-constructors)))))

         (define (scfg->pl scfg)
           ;; 1 clause per production

           ;; several names associated with each nt.
           (define (nt-name->pred-name nt-name)
             (string->symbol (string-append "pred_" (symbol->string (nt-name->symbol nt-name)))))

           (define (nt-name->subtree_name nt-name i)
             (string->symbol (string-append "Subtrees_" 
                                            (number->string i) 
                                            "_" 
                                            (symbol->string (nt-name->symbol nt-name)))))

           (define (nt-name->tree-name nt-name i)
             (string->symbol (string-append "tree_" 
                                            (number->string i)
                                            "_"
                                            (symbol->string (nt-name->symbol nt-name)))))

           (define (nt-name->choices nt-name)
             (prod->choices (name->prod nt-name)))

           ;; <pred-name>(<pred-args>) :- findall(Tree, (<disj over choices>, Tree = <tree-name>(<all subtrees in choices)

           (define (sexp-walk f expr)
             (begin 
               ;; (print "sexpr-walk")
               ;; (pretty-print expr)
               (cond [(null? expr) expr]
                     [(list? expr) (let* ([new-expr (f expr)])
                                     (cond [(list? new-expr)
                                            (cons (sexp-walk f (car new-expr))
                                                  (sexp-walk f (cdr new-expr)))]
                                           [else new-expr]))]

                     [else expr])))

           (define (body->conjunction nt-name idx-body num-choices) 
             (define all-nt-names  (scfg->nonterminal-names scfg))

             (define (body->named-occurrences body)
               (define occurrences (make-hash-table equal?))
               (define (init-occurrences)
                 (begin (for-each (lambda (name)
                                    (hash-table-set! occurrences name 0))
                                  all-nt-names)))

               (define (increment-occurrences expr)
                 (if (hash-table-exists? occurrences expr)
                   (begin 
                     ;; (print "Exists: ~s" expr)
                     (hash-table-set! occurrences expr (+ 1 (hash-table-ref occurrences expr)))
                     (list (car expr) (hash-table-ref occurrences expr)))
                   expr))

               (begin 
                 (init-occurrences)
                 (list (sexp-walk (lambda (t) (increment-occurrences t)) body)
                       (concatenate (map (lambda (ni) (map (lambda (j) (list (first ni) (+ 1 j))) (iota (second ni)))) 
                                         (filter (lambda (x) (> (second x) 0)) (map (lambda (ni) (list (car (first ni)) (cdr ni))) 
                            (hash-table->alist occurrences))))))))

             (define (occurrence->var name-idx)
               (string->symbol 
                 (string-append "Var_"
                                (number->string (second name-idx))
                                "_"
                                (symbol->string (first name-idx)))))

             (define (occurrence->subtree-var name-idx)
               (string->symbol 
                 (string-append "Subtree_"
                                (number->string (second name-idx))
                                "_"
                                (symbol->string (first name-idx)))))

             (define (to-term-predicate body-nt-occurrences)
               (begin 
                 ;; (print "all nt names: ~s" all-nt-names)
                 ;; (pretty-print body-nt-occurrences)
                 ;; (print "sexp-walk result: ")
                 ;; (pretty-print (sexp-walk (lambda (t) (cond [(and (list? t) (> (length t) 1) (contains? (list (car t)) all-nt-names) (number? (second t))) (occurrence->var t)]
                                                                 ;; [else t]))
                                               ;; (first body-nt-occurrences)))
                 (pl-relation '= 'X (sexp-walk (lambda (t) (cond [(and (list? t) (> (length t) 1) (contains? (list (car t)) all-nt-names) (number? (second t))) (occurrence->var t)]
                                                                 [else t]))
                                               (first body-nt-occurrences))
                              )))

             (define (to-nt-children-predicates body-nt-occurrences)
               (define occurrence-list (second body-nt-occurrences))
               (define (occurrence->children-pred occurrence)
                 (define name-of-interest (list (car occurrence)))
                 (define var (occurrence->var occurrence))
                 (define subtree-var (occurrence->subtree-var occurrence))
                 (pl-relation (nt-name->pred-name name-of-interest)
                              var
                              subtree-var))
               (begin ;; (print "to-nt-children-predicates: occurrence list:")
                      ;; (pretty-print occurrence-list)
                      (map occurrence->children-pred occurrence-list)))

             (define (to-tree-pred nt-name body-nt-occurrences)
               (define occurrence-list (second body-nt-occurrences))
               ;; (define tree-sym (nt-name->tree-name nt-name (first idx-body)))
               (define tree-sym 'tree)
               (define tree-term (cond [(null? occurrence-list)
                                        (pl-relation tree-sym (string-append "sym_" (symbol->string (car nt-name))) (first idx-body) num-choices)]
                                       [else (apply (curry pl-relation tree-sym)
                                                    (append (list (string-append "sym_" (symbol->string (car nt-name))) (first idx-body) num-choices) (map occurrence->subtree-var occurrence-list)))]))
               (pl-relation '= 'Tree tree-term))

             ;; (X = body-with-replaced-nts
             (let* ([idx (first idx-body)]
                    [body (second idx-body)]
                    ;; format of body-term:
                    ;; (list <body with replaced nts> <the named occurrences>)
                    [body-nt-occurrences (body->named-occurrences body)] 
                    [x-equals-body (to-term-predicate body-nt-occurrences)]
                    [nt-children-predicates (to-nt-children-predicates body-nt-occurrences)]
                    [tree-pred (to-tree-pred nt-name body-nt-occurrences)])
               (apply pl-conj (append (list x-equals-body) nt-children-predicates (list tree-pred)))))

           (define (prod->chart-predicate prod)
             (let* ([nt-name (prod->name prod)]
                    [pred-name (nt-name->pred-name nt-name)]
                    [choices (prod->choices prod)]
                    [idx-choices (zip (iota (length choices)) choices)]
                    [disjunction (apply pl-disj (map (lambda (idx-choice) (body->conjunction nt-name idx-choice (length idx-choices))) idx-choices))])
               (pl-clause (pl-relation pred-name 'X 'Trees) 
                          (pl-relation 'find_at_least_one
                                       'Tree
                                       disjunction
                                       'Trees))))

           (map prod->chart-predicate (scfg->productions scfg))
           )


         ;; returns a tree
         (define (run-chart-parse scfg term)
           (define query (pl-clause (pl-relation 'go)
                                    (pl-relation 'test_data term)))

           (define scheme-header "chart-parse-header.ss")
           (define scheme-result-file "chart-parse-out.ss")

           (define (create-pl-from-header header-name scfg out-name)
             (begin 
               (system (format "rm ~s" out-name))
               (with-output-to-file out-name 
                                    (lambda () (begin (for-each (lambda (p) (begin (display p)
                                                                                   (newline)))
                                                                (scfg->pl scfg))
                                                      (display query)
                                                      (newline))))
               (system (format "cat ~s >> ~s" header-name out-name))
               
               )
             )
           (begin
             (create-pl-from-header "chart-parsing-header.pl" scfg "chart-parse-tmp.pl")
             ;; B-Prolog
             ;; (system (format "bp -g ~s" (format "consult(~s),go" (format "chart-parse-tmp.pl"))))
             (system (format "swipl -s ~s -q -t go." "chart-parse-tmp.pl"))
             (read (open-input-file scheme-result-file))
             )
           )


         )

