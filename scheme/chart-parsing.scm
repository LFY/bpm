(library (chart-parsing)
         (export program->scfg
                 scfg->pl
                 run-chart-parse
                 
                 replace-choices
                 
                 to-lowercase-symbol
                 uppercase-symbol?
                 
                 batch-run-chart-parse
                 batch-run-inversion)

         (import (except (rnrs) string-hash string-ci-hash)
                 (rnrs eval)
                 (rnrs io ports)
                 (program)
                 (sym)
                 (util)
                 (printing)
                 (_srfi :1)
                 (_srfi :69)
                 ;; (srfi :13)

                 (named-search-trees)
                 (prolog-serialize)
                 (scfg)
                 (only (scheme-tools) system)
                 (church external py-pickle)
                 
                 (program-pl)
                 )

         (define (replace-choices expr)
           (subexpr-walk (lambda (t) (cond 
                                       [(and (list? t) (eq? 'choose (car t))) (replace-car t 'nondet-choice)]
                                       [else t])) expr))

         (define (nondet-desugar-body body)
           `(define-nondet (start) ,(replace-choices (caddr body))))

         (define (nondet-desugar-abstraction abstr)
           `(define-nondet ,(cons (abstraction->name abstr)
                                  (abstraction->vars abstr))
                           ,(replace-choices (abstraction->pattern abstr))))

         (define (nondet-desugar-abstractions prog)
           (map nondet-desugar-abstraction (program->abstractions prog)))

         (define (uppercase-symbol? t)
           (and (symbol? t)
                (char-upper-case? (string-ref (symbol->string t) 0))))

         (define (to-lowercase-symbol t)
           (string->symbol (string-append (string-downcase (symbol->string t)) "Sym")))

         (define extra-defines '())

         (define (gen-extra-defines prog)

           (define (is-new-app? e) (and (non-empty-list? e) 
                                        (symbol? (car e)) 
                                        (not (contains? (car e)
                                                        (append (map abstraction->name (program->abstractions prog))
                                                                '(program lambda define choose)
                                                                (map cadr extra-defines))))))

           (define (get-extra-defines expr)
             (subexpr-walk (lambda (t) (cond [(is-new-app? t) (begin (set! extra-defines (cons `(define-constr ,(car t)) extra-defines))
                                                                     t)]
                                             [else t]))
                           expr))

           (map get-extra-defines (cons (program->body prog) (map abstraction->pattern (program->abstractions prog)))))

         (define (program->scfg prog) 
           (let* ([none (if (null? extra-defines) (gen-extra-defines prog))]
                  [desugared-body (nondet-desugar-body (program->body prog))]
                  [desugared-abstractions (nondet-desugar-abstractions prog)]
                  [desugared-prog `((lambda ()
                                      ,@extra-defines
                                      ,@desugared-abstractions 
                                      ,desugared-body
                                      (nondet-program->named-search-tree start)))] 
                  ;; [db (begin (pretty-print prog)
                             ;; (pretty-print desugared-prog))]
                  )
             (eval desugared-prog
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

           (define (body->conjunction nt-name idx-body num-choices) 
             (define all-nt-names  (scfg->nonterminal-names scfg))

             (define (body->named-occurrences body)
               (define occurrences (make-hash-table equal?))
               (define (init-occurrences)
                 (begin (for-each (lambda (name)
                                    (hash-table-set! occurrences name 0))
                                  all-nt-names)))

               (define (increment-occurrences expr)
                 (begin ;; (print "Expr in question:")
                        ;; (pretty-print expr)
                 (if (hash-table-exists? occurrences expr)
                   (begin 
                     ;; (print "Exists: ~s" expr)
                     (hash-table-set! occurrences expr (+ 1 (hash-table-ref occurrences expr)))
                     (list (car expr) (hash-table-ref occurrences expr)))
                   (begin ;; (print "this expr does not occur")
                            expr))))

               (begin 
                 ;; (pretty-print body)
                 (init-occurrences)
                 ;; (print "after init occurrences")
                 (list (sexp-walk (lambda (t) (increment-occurrences t)) body)
                       
                       (begin ;; (print "after increment occurrences")
                              ;; (print (hash-table->alist occurrences))
                       
                       
                       
                       (concatenate (map (lambda (ni) (map (lambda (j) (list (first ni) (+ 1 j))) (iota (second ni)))) 
                                         (filter (lambda (x) (> (second x) 0)) (map (lambda (ni) (list (car (first ni)) (cdr ni))) 
                            (hash-table->alist occurrences))))))
                       
                       
                       )))

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
                                                                 [(uppercase-symbol? t) (to-lowercase-symbol t)]
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
               (define tree-sym 'parse_node)
               (define tree-term (cond [(null? occurrence-list)
                                        (pl-relation tree-sym 
                                                     (car nt-name)
                                                     ;; (string-append "sym_" (symbol->string (car nt-name))) 
                                                     (first idx-body) num-choices)]
                                       [else (apply (curry pl-relation tree-sym)
                                                    (append (list 
                                                              (car nt-name)
                                                              ;; (string-append "sym_" (symbol->string (car nt-name))) 
                                                              (first idx-body) num-choices) (map occurrence->subtree-var occurrence-list)))]))
               (pl-relation '= 'Tree tree-term))

             ;; (X = body-with-replaced-nts
             (let* ([idx (first idx-body)]
                    ;; [db (print "body->conjunction after idx")]
                    [body (second idx-body)]
                    ;; [db (print "body->conjunction after body")]
                    ;; format of body-term:
                    ;; (list <body with replaced nts> <the named occurrences>)
                    [body-nt-occurrences (body->named-occurrences body)] 
                    ;; [db (print "body->conjunction after body-nt-occurrences")]
                    [x-equals-body (to-term-predicate body-nt-occurrences)]
                    ;; [db (print "body->conjunction after x-equals-body")]
                    [nt-children-predicates (to-nt-children-predicates body-nt-occurrences)]
                    ;; [db (print "body->conjunction after nt-children-predicates")]
                    [tree-pred (to-tree-pred nt-name body-nt-occurrences)]
                    ;; [db (print "body->conjunction after tree-pred")]
                    )
               (apply pl-conj (append (list x-equals-body) 
                                      nt-children-predicates 
                                      (list tree-pred) 
                                      (list (pl-relation 'add_if_not_present 'Tree 'TreeID))
                                      ))))

           (define (prod->chart-predicate prod)
             (let* ([nt-name (prod->name prod)]
                    ;; [db (print "after nt-name")]
                    [pred-name (nt-name->pred-name nt-name)]
                    ;; [db (print "after pred-name")]
                    [choices (prod->choices prod)]
                    ;; [db (print "after choices")]
                    [idx-choices (zip (iota (length choices)) choices)]
                    [db (if (= 0 (length idx-choices)) 
                          (begin (pretty-print scfg)
                                 (print "after idx-choices")))]
                    [disjunction (apply pl-disj (map (lambda (idx-choice) (body->conjunction nt-name idx-choice (length idx-choices))) idx-choices))]
                    ;; [db (begin (print "after disj")
                               ;; (print disjunction))]
                    
                    )
               (pl-clause (pl-relation pred-name 'X 'TreeIDs) 
                          (pl-relation 'find_at_least_one
                                       'TreeID
                                       disjunction
                                       'TreeIDs))))

           (let* ([productions (scfg->productions scfg)]
                  ;; [db (begin (print "scfg:")
                             ;; (pretty-print scfg)
                             ;; (print "productions:")
                             ;; (pretty-print productions))]
                  )
           (map prod->chart-predicate productions))
           )

            (define chart-parsing-header "
              :- use_module(library(gensym)).
              find_at_least_one(X, Y, Z) :- findall(X, Y, Z), length(Z, N), N > 0.
            :- dynamic dag/2.
            :- dynamic feature/2.
            :- tell('chart-parse-out.ss').

            add_if_not_present(Term, ID) :- (dag(ID, Term)) -> (true) ; (gensym('', ID), assertz(dag(ID, Term))).

            write_dags(TopLevel) :- find_at_least_one(DagNode, (dag(ID, Tree), retract(dag(ID, Tree)), DagNode = [ID, Tree]), DagNodes), term2sexpr([TopLevel, DagNodes]).

            write_features :- asserta(feature(end, end)), find_at_least_one(DagNode, (feature(ID, Tree), retract(feature(ID, Tree)), DagNode = [ID, Tree]), DagNodes), DagNodes = [_ | Actual], term2sexpr(features(Actual)).

            open_paren :- write('(').
            close_paren :- write(')').

            term2sexpr([X|Xs]) :- write('('), term2sexpr(X), map_term2sexpr(Xs), write(') ').
            term2sexpr(T) :- T =.. L, L = [F|[]], write(F), write(' ').
            term2sexpr(T) :- T =.. L, L = [F|Xs], write('('), write(F), write(' '), map_term2sexpr(Xs), write(') ').

            map_term2sexpr([]) :- true.
            map_term2sexpr([X|Xs]) :- term2sexpr(X), map_term2sexpr(Xs).")

            (define pl-tmp-name "chart-parse-tmp.pl")

        ;; batch data examples as well.
        ;; returns: a list of lists of parse trees, (per-scfg (per-term))
        (define (batch-run-chart-parse scfgs terms)
          (define prefixes (map (lambda (i) (string-append "scfg_" (number->string i) "_")) (iota (length scfgs))))
          (define term-ids (map (lambda (i) (string-append "data_" (number->string i) "_")) (iota (length terms))))

          (define (scfg->sym-string scfg)
            (symbol->string (car (scfg->start-name scfg))))

          (define (scfg->test-pred-name scfg)
            (string-append "test_data_" (scfg->sym-string scfg)))
          (define (scfg->pred-start-name scfg)
            (string-append "pred_" (scfg->sym-string scfg)))
          (define (scfg->query-names scfg) (map (lambda (term-id) (string-append "query_" term-id (scfg->sym-string scfg)))
                                          term-ids))
          (define (scfg->runall-name scfg)
            (string-append "runall_" (scfg->sym-string scfg)))

          (define prefixed-scfgs (map prefix-scfg prefixes scfgs))

          (define (make-one-pl scfg)
            (let* ([test-pred-name (scfg->test-pred-name scfg)]
                   [pred-start-name (scfg->pred-start-name scfg)]
                   [query-names (scfg->query-names scfg)]
                   [runall-name (scfg->runall-name scfg)])
              (cons (pl-clause (pl-relation runall-name)
                               'open_paren (apply pl-conj query-names) 'close_paren)
              (cons (pl-clause (pl-relation test-pred-name 'Data)
                               (pl-relation 'retractall 'dag)
                               (pl-ifte (pl-relation pred-start-name 'Data 'Result)
                                        (pl-relation 'write_dags 'Result)
                                        (pl-relation 'term2sexpr (pl-list '())))
                               'nl)
                    (append (map (lambda (query-name term)
                                   (pl-clause (pl-relation query-name)
                                              (pl-relation test-pred-name
                                                           (sexp-walk (lambda (t) (cond [(uppercase-symbol? t) (to-lowercase-symbol t)]
                                                                                        [else t]))
                                                                      term))))
                                 query-names terms)
                            (scfg->pl scfg))))))

          (define global_run
            (pl-clause (pl-relation 'run_everything)
                       'open_paren (apply pl-conj (map scfg->runall-name prefixed-scfgs)) 'close_paren))
         
          (let* ([scfg-pl (cons global_run (concatenate (map make-one-pl prefixed-scfgs)))])
            (begin (system (format "rm ~s" pl-tmp-name))
                   (with-output-to-file 
                     pl-tmp-name 
                     (lambda () (begin (print chart-parsing-header)
                                       (display-pl scfg-pl))))
                   (system (format "swipl -L0 -O -qs ~s -t run_everything." pl-tmp-name))
                   (read (open-input-file "chart-parse-out.ss")))))

        (define (batch-run-inversion progs terms . features?)
          (define prefixes (map (lambda (i) (string-append "prog_" (number->string i) "_")) (iota (length progs))))
          (define term-ids (map (lambda (i) (string-append "data_" (number->string i) "_")) (iota (length terms))))

          (define (prog->start-name prog) "TopLevel")

          (define (sym-string prefix)
            (string-append prefix "TopLevel"))

          (define (test-pred-name prefix)
            (string-append "test_data_" (sym-string prefix)))

          (define (pred-start-name prefix)
            (cond [(null? features?)
                   (string-append "p" (sym-string prefix))]
                  [else (sym-string prefix)]))

          (define (query-names prefix) 
            (map (lambda (term-id) (string-append "query_" term-id (sym-string prefix)))
                 term-ids))

          (define (runall-name prefix)
            (string-append "runall_" (sym-string prefix)))

          (define prefixed-pls (map (lambda (prefix prog) (
                                                           (cond [(null? features?) program->pl]
                                                                 [else program->pl+features])
                                                           prog prefix))
                                    prefixes progs))

          (define (make-one-pl prefix prefixed-pl)
            (let* ([test-pred-name (test-pred-name prefix)]
                   [pred-start-name (pred-start-name prefix)]
                   [query-names (query-names prefix)]
                   [runall-name (runall-name prefix)])
              (cons (pl-clause (pl-relation runall-name)
                               'open_paren (apply pl-conj query-names) 'close_paren)
              (cons (cond [(null? features?)
                           (pl-clause (pl-relation test-pred-name 'Data)
                               (pl-relation 'retractall 'dag)
                               (pl-ifte (pl-relation pred-start-name 'Data 'Result)
                                        (pl-relation 'write_dags 'Result)
                                        (pl-relation 'term2sexpr (pl-list '())))
                               'nl)]
                          [else (pl-clause (pl-relation test-pred-name 'Data)
                                           (pl-relation 'retractall 'dag)
                                           (pl-relation 'retractall 'feature)
                                           (pl-ifte (pl-relation pred-start-name 'Data 'Result)
                                                    (pl-conj 'open_paren
                                                             (pl-relation 'write_dags 'Result)
                                                             (pl-relation 'write_features)
                                                             'close_paren)
                                                    (pl-relation 'term2sexpr (pl-list '())))
                                           'nl) ])
                    (append (map (lambda (query-name term)
                                   (pl-clause (pl-relation query-name)
                                              (pl-relation test-pred-name
                                                           (sexp-walk (lambda (t) (cond [(uppercase-symbol? t) (to-lowercase-symbol t)]
                                                                                        [else t]))
                                                                      term))))
                                 query-names terms)
                            prefixed-pl)))))

          (define global_run
            (pl-clause (pl-relation 'run_everything)
                       'open_paren (apply pl-conj (map runall-name prefixes)) 'close_paren))
         
          (let* ([prog-pl (cons global_run (concatenate (map make-one-pl prefixes prefixed-pls)))])
            (begin (system (format "rm ~s" pl-tmp-name))
                   (with-output-to-file 
                     pl-tmp-name 
                     (lambda () (begin (print chart-parsing-header)
                                       (display-pl prog-pl)

                                       (display ":- run_everything.") ;; YAP-specific
                                       
                                       )))
                   ;; (system (format "swipl -L0 -O -qs ~s -t run_everything." pl-tmp-name))
                   ;; (system (string-append "bp -g \"consult('" pl-tmp-name "'),run_everything.\""))
                   ;; (system (format "swipl -L0 -O -qs ~s -t run_everything." pl-tmp-name))
                   (system (string-append "yap -L " pl-tmp-name))

                   (read (open-input-file "chart-parse-out.ss")))))

         (define (run-chart-parse scfg term)

           (define test-pred-name (string->symbol (string-append "test_data_" (symbol->string (car (scfg->start-name scfg))))))
           (define start-name (string->symbol (string-append "pred_" (symbol->string (car (scfg->start-name scfg))))))

           (define test-pred (pl-clause (pl-relation test-pred-name 'Data)
                                        (pl-relation 'retractall 'dag)
                                        (pl-ifte (pl-relation start-name 'Data 'Result)
                                                 (pl-relation 'write_dags 'Result)
                                                 (pl-relation 'term2sexpr (pl-list '())))
                                        'nl))

           (define query (pl-clause (pl-relation 'go)
                                    (pl-relation test-pred-name
                                                 (sexp-walk (lambda (t) (cond [(uppercase-symbol? t) (to-lowercase-symbol t)]
                                                                              [else t]))
                                                            term))))

           (define (create-pl scfg)
             (begin 
               (system (format "rm ~s" pl-tmp-name))
               (with-output-to-file 
                 pl-tmp-name 
                 (lambda () (begin (print chart-parsing-header)
                                   (display-pl (cons query (cons test-pred (scfg->pl scfg)))))))
               )
             )

           (begin
             ;; (print "starting chart parse")
             ;; (pretty-print scfg)
             (create-pl scfg)
             ;; (print "created pl")
             (system (format "swipl -L0 -O -qs ~s -t go." pl-tmp-name))
             ;; (print "ran swipl")
             (read (open-input-file "chart-parse-out.ss"))
             ;; (print "end chart parse")
             )
           )
)
