(library (program-pl)
         (export program->pl
                 program->pl+features

                 program->anf
                 anf-eval
                 anf-abstr->equations
                 ;; debug
                 normalized-abstr-eqs?
                 contains-choice?
                 choice-top-level?
                 var-sym?

                 finalize-relations
                 distribute-choices
                
                 remove-unused-variables
                 local-remove-dead-code
                 dead-code-elimination
                 )
         (import (program)
                 (except (rnrs) string-hash string-ci-hash)
                 (_srfi :1)
                 (_srfi :69)
                 (util)
                 (printing)
                 (prolog-serialize)
                 (delimcc-simple-ikarus))

         (define (sym-append . syms)
           (string->symbol (apply string-append (map (lambda (s) (symbol->string s)) syms))))

         (define (var-sym? e)
           (and (symbol? e)
                (contains?
                  (substring (symbol->string e)
                             0 1)
                  '("X" "V"))))

         (define (ground? e)
           (and (not (list? e))
                (or (number? e)
                    (string? e)
                    (and (symbol? e) not (var-sym? e)))))

         (define var-sym-counter 0)
         (define (new-var-sym)
           (let* ([answer (string->symbol (string-append "X" (number->string var-sym-counter)))])
             (begin (set! var-sym-counter (+ 1 var-sym-counter))
                    answer)))

         (define subtreeid-sym-counter 0)
         (define (new-subtree-sym)
           (let* ([answer (string->symbol (string-append "SubTrIDs" (number->string subtreeid-sym-counter)))])
             (begin (set! subtreeid-sym-counter (+ 1 subtreeid-sym-counter))
                    answer)))

         (define choice-sym-counter 0)
         (define (new-choice-sym)
           (let* ([answer (string->symbol (string-append "C" (number->string choice-sym-counter)))])
             (begin (set! choice-sym-counter (+ 1 choice-sym-counter))
                    answer)))


         (define my-tree-sym 'Tree)
         (define my-treeid-sym 'TreeID)
         (define my-treeids-sym 'TreeIDs)

         (define (mk-let bindings body)
           `(let ,bindings ,body))
         (define (let? e) (and (list? e) (eq? 'let (car e))))
         (define let->bindings second)
         (define let->body third)

         (define (lambda? e) (and (list? e) (eq? 'lambda (car e))))
         (define (background-application? e)
           (and (list? e) 
                (not (lambda? (car e)))))

         (define (lambda-application? e)
           (and (list? e)
                (list? (car e))
                (eq? 'lambda (caar e))))

         (define (choice? rhs)
           (and (list? rhs) (eq? 'choose (car rhs))))

         (define (hash-table->pairlist table) (map (lambda (xy) (list (car xy) (cdr xy))) (hash-table->alist table)))

         (define (anf-eval abstr-names abstr)
           ;; a table of abstraction names to true or false depending on whether they syntactically exhibit nondeterminism
           (define syntactic-nondet-table (make-hash-table equal?))

           (define (arg->binding a)
             `(,(new-var-sym) ,(anf-convert a)))

           (define (abstraction->result-name abstr)
             (string->symbol (string-append (symbol->string (abstraction->name abstr)) "_res")))

           (define (anf-convert bound-vars expr)

             (define (application? e) (and (list? e) (or (lambda? (car e))
                                                         (contains? (car e) abstr-names))))
             (define (administrative? arg)
               (or (contains? arg bound-vars)
                   (number? arg)
                   (string? arg)
                   (boolean? arg)))

             (define (not-same? ab)
               (not (eq? (car ab) (cadr ab))))

             (define (mk-bindings args)
               (map (lambda (arg) (cond [(administrative? arg) `(,arg ,arg)]
                                        [else `(,(new-var-sym) ,(anf-convert bound-vars arg))]))
                    args))

             (cond [(choice? expr)
                    (let* ([alternatives (cdr expr)])
                      `(choose ,@(map (lambda (anf)
                                        (cond [(or (choice? anf) (let? anf))
                                               ;; introduce new top-level-var for each alternative
                                               (let* ([new-top-level-var (new-var-sym)])
                                                 (mk-let `([,new-top-level-var ,anf])
                                                         new-top-level-var))]
                                              [else anf]
                                              ))
                                      (map (curry anf-convert bound-vars) alternatives))))]
                   [(background-application? expr)
                    (let* ([func (car expr)]
                           [args (cdr expr)]
                           [bindings (mk-bindings args)]
                           [true-bindings (filter not-same? bindings)]
                           )
                      (cond [(or (null? args)
                                 (null? true-bindings)) expr]
                            [else
                              (mk-let true-bindings 
                                      `(,func
                                         ,@(map car bindings)))]))]
                   [(lambda-application? expr)
                    (let* ([lambda-term (car expr)]
                           [lambda-vars (cadr lambda-term)]
                           [lambda-body (caddr lambda-term)]
                           [args (cdr expr)]
                           [bindings (zip lambda-vars (map (curry anf-convert bound-vars) args))])
                      (cond [(or (null? args)
                                 (null? bindings)) expr]
                            [else (mk-let bindings
                                          (anf-convert (append lambda-vars bound-vars) lambda-body))]))]
                   [else expr]))

           (make-named-abstraction (abstraction->name abstr)
                                   (mk-let `([,(abstraction->result-name abstr) ,(anf-convert (abstraction->vars abstr) (abstraction->pattern abstr))]) (abstraction->result-name abstr))
                                   (abstraction->vars abstr)))

         (define (app-of-existing-abstr? abstr-names expr)
           (contains? (car expr) abstr-names))

         (define (local-evidence-push abstr-names equations)
           (define (choice-push equations)
             (let*-values
               ([(has-choice?)
                 (lambda (eq)
                   (and (list? (eq->rhs eq))
                        (cond [(choice? (eq->rhs eq)) #t]
                              [else #f])))]
                [(with without) (partition has-choice? equations)])
               (append without with)))
           (let*-values 
             ([(has-application?)
               (lambda (eq)
                 (and (list? (eq->rhs eq))
                      (cond [(choice? (eq->rhs eq)) #t]
                            [(app-of-existing-abstr? abstr-names (eq->rhs eq)) #t]
                            [else #f])))]
              [(with-app without-app)
               (partition has-application? equations)])
             (append without-app (choice-push with-app))))

         ;; probably the worst implementation of let flattening in the entire universe
         (define (anf-abstr->equations abstr-names anf-abstr)
           (letrec* ([loop (lambda (curr-let)
                             (cond  [(let? curr-let)
                                     (letrec* ([my-bindings (let->bindings curr-let)]
                                               [find-body (lambda (def)
                                                            (cond [(choice? def)
                                                                   `(choose ,@(map (lambda (e) (cond [(or (choice? e) (let? e)) (loop e)]
                                                                                                     [else e])) (cdr def)))]

                                                                  [(let? def) (find-body (let->body def))]
                                                                  [else def]))]
                                               [my-eqs (map (lambda (b) (list '= (car b) (find-body (cadr b)))) 
                                                            my-bindings)])
                                              (concatenate
                                                (cons my-eqs 
                                                      (map loop (append (map cadr my-bindings) (list (let->body curr-let)))))))]

                                    [else '()]))])
                    (let* ([abstr-name (abstraction->name anf-abstr)]
                           [abstr-vars (abstraction->vars anf-abstr)]
                           [top-level-var (caar (let->bindings (abstraction->pattern anf-abstr)))])
                      `(abstr-eqs ,abstr-name 
                                  ,abstr-vars 
                                  ,top-level-var 
                                  ,(local-evidence-push abstr-names (loop (abstraction->pattern anf-abstr)))))))

         (define abstr-eqs->name cadr)
         (define abstr-eqs->vars caddr)
         (define abstr-eqs->top-level-var cadddr)
         (define (abstr-eqs->eqs aeqs) (cadddr (cdr aeqs)))

         (define (abstr-eqs? expr) (and (list? expr) (eq? 'abstr-eqs (car expr))))
         (define (mk-abstr-eqs name vars tlv eqs)
           `(abstr-eqs ,name ,vars ,tlv ,eqs))

         (define (replace-abstr-eqs-body abstr-eqs new-body)
           (mk-abstr-eqs (abstr-eqs->name abstr-eqs)
                         (abstr-eqs->vars abstr-eqs)
                         (abstr-eqs->top-level-var abstr-eqs)
                         new-body))

         (define eq->lhs cadr)
         (define eq->rhs caddr)
         (define (equation? e) (and (list? e) (eq? '= (car e))))

         (define (choice-top-level? abstr-eqs) 
           (eq? 'choose (car (eq->rhs (car (abstr-eqs->eqs abstr-eqs))))))

         (define (eqs-w/-top-level-choice->choices abstr-eqs)
           (cdr (eq->rhs (car (abstr-eqs->eqs abstr-eqs)))))

         (define (contains-choice? t) (not (null? (deep-find-all choice? t))))

         (define (normalized-abstr-eqs? abstr-eqs) 
           (let* ([equations (abstr-eqs->eqs abstr-eqs)])
             (if (contains-choice? equations)
               (if (choice-top-level? abstr-eqs)
                 (let* ([res (disj (map contains-choice?
                                        (eqs-w/-top-level-choice->choices abstr-eqs)))])
                   (not res))
                 #f)
               #t)))

         (define (all-vars t)
           (define answer '())
           (sexp-walk (lambda (e) (cond [(var-sym? e) (begin (set! answer (cons t answer)) e)]
                                        [else e]))
                      t))

         (define (mk-new-choice-pred abstr-eqs t)
           (let* ([name (new-choice-sym)]
                  ;; something we'd like to be true: that all vars found in the choice term are not defined in anything "higher up"
                  [vars (lset-intersection equal?  ;; doesn't work in general, for locally defined lambdas. maybe it's time to do lambda lifting?
                                           (abstr-eqs->vars abstr-eqs)
                                           (all-vars t))]
                  [new-tlv (string->symbol (string-append (symbol->string name) "_topLevel"))])
             (mk-abstr-eqs name vars new-tlv `((= ,new-tlv ,t)))))

         (define (normalize-choices abstr-eqs)
           (define new-choice-preds '())

           (define (normalize-step curr-abstr-eqs expr) ;; expr is either an AbstrEq or a list of equations
             (cond [(abstr-eqs? expr) 
                    (cond [(normalized-abstr-eqs? expr) expr]
                          [(choice-top-level? expr)
                           (let* ([choices (eqs-w/-top-level-choice->choices expr)])
                             (replace-abstr-eqs-body expr 
                                                     `((= ,(abstr-eqs->top-level-var expr)
                                                          (choose ,@(map (curry normalize-step expr) choices))))))]
                          [else (replace-abstr-eqs-body expr
                                                        (normalize-step expr (abstr-eqs->eqs expr)))])]
                   [else (subexpr-walk (lambda (t) 
                                         (cond [(and (equation? t)
                                                     (choice? (eq->rhs t)))
                                                (let* ([new-choice-pred (mk-new-choice-pred curr-abstr-eqs (eq->rhs t))]
                                                       [name (abstr-eqs->name new-choice-pred)]
                                                       [vars (abstr-eqs->vars new-choice-pred)]
                                                       [final-new-choice-pred (normalize-step new-choice-pred new-choice-pred)])
                                                  (begin (set! new-choice-preds 
                                                           (cons final-new-choice-pred new-choice-preds))
                                                         `(= ,(eq->lhs t) (,name ,@vars))))]
                                               [else t]))
                                       expr)]))

           (let* ([new-abstr-eqs (normalize-step abstr-eqs abstr-eqs)])
             (append (list new-abstr-eqs) new-choice-preds)))

         (define (distribute-choices normalized-eqs)
           (cond [(contains-choice? normalized-eqs)
                  (let* ([tlv (abstr-eqs->top-level-var normalized-eqs)]
                         ;; the rule:
                         ;; (= v1 (choose <a1> <a2> ...)) ===
                         ;; (choose (= v1 <a1>) (= v1 <a2>))...

                         [revise-choice (lambda (alt)
                                          (cond [(and (list? alt) (equation? (car alt)))
                                                 (let* ([first-eq (car alt)]
                                                        [new-first-eq `(= ,tlv ,(eq->rhs first-eq))])
                                                   (cons new-first-eq (cdr alt)))]
                                                [else `((= ,tlv ,alt))]))])
                    (replace-abstr-eqs-body normalized-eqs
                                            `(choose ,@(map revise-choice (eqs-w/-top-level-choice->choices normalized-eqs)))))]
                 [else normalized-eqs]))

         ;; adding arguments for subtrees and tree ID's
         ;; by now, we are guaranteed that each predicate is in the normal form

         (define (finalize-relations all-normalized-eqs . no-trees)

           (define abstr-names (map abstr-eqs->name all-normalized-eqs))

           (define (abstr-name->pred-name abstr-name)
             (sym-append 'p abstr-name))

           (define (nondet? eqs) (eq? 'choose (car (abstr-eqs->eqs eqs))))

           (define (distributed-nondet-eqs->choices eqs)
             (cdr (abstr-eqs->eqs eqs)))


           (define (body->relations pred-name choice-index num-choices pred-vars body no-tree-preds)
             (define my-subtrees '())
             (define (constructor? rhs) 
               (and (list? rhs) (not (contains? (car rhs) abstr-names))))

             (define (application? rhs)
               (and (list? rhs) (contains? (car rhs) abstr-names)))

             (define (higher-order-application? rhs)
               (and (list? rhs) (var-sym? (car rhs))))

             (define (transform-eq eq)
               (cond 
                 [(application? (eq->rhs eq)) 
                  (let* ([renamed-rhs (cons (abstr-name->pred-name (car (eq->rhs eq)))
                                            (cdr (eq->rhs eq)))])
                    (append renamed-rhs (list (eq->lhs eq)) 
                            (cond [(null? no-trees) 
                                   (let* ([subtree-name (new-subtree-sym)])
                                     (begin (set! my-subtrees (cons subtree-name my-subtrees))
                                            (list subtree-name))
                                     )]
                                  [else '()])
                            ))]
                 [(higher-order-application? (eq->rhs eq))
                  (let* ([renamed-rhs (cons 'call (eq->rhs eq))])
                    (append renamed-rhs (list (eq->lhs eq)) 
                            (cond [(null? no-trees) 
                                   (let* ([subtree-name (new-subtree-sym)])
                                     (begin (set! my-subtrees (cons subtree-name my-subtrees))
                                            (list subtree-name))
                                     )]
                                  [else '()])
                            ))]
                 [else eq]))

             (let* ([transformed-eqs (map transform-eq body)])
               (append transformed-eqs
                       (cond [(and (not no-tree-preds) (null? no-trees))
                              (list `(= Tree 
                                        (tree ,pred-name ,choice-index ,num-choices
                                              (vars ,@pred-vars)
                                              ,@my-subtrees))
                                    '(add_if_not_present Tree TreeID))]
                             [else '()]

                             ))))

           (define (transform-one-alternative choice-index num-choices abstr-eqs body no-tree-preds)
             (let* ([relations (body->relations (abstr-name->pred-name (abstr-eqs->name abstr-eqs)) 
                                                choice-index 
                                                num-choices
                                                (abstr-eqs->vars abstr-eqs) 
                                                body
                                                no-tree-preds)])
               `(conj ,@relations)))

           (define (transform-body abstr-eqs no-tree-preds)
             (cond [(nondet? abstr-eqs)
                    (let* ([choices (distributed-nondet-eqs->choices abstr-eqs)]
                           [num-choices (length choices)]
                           [alternative->relation (lambda (idx alt) (transform-one-alternative idx
                                                                                               num-choices
                                                                                               abstr-eqs
                                                                                               alt
                                                                                               no-tree-preds))])
                    `(disj ,@(map alternative->relation (iota num-choices) (distributed-nondet-eqs->choices abstr-eqs))))]
                   [else (transform-one-alternative 0 1 abstr-eqs (abstr-eqs->eqs abstr-eqs) no-tree-preds)]))
           
           (define (is-top-level? abstr-eqs)
             (eq? 'TopLevel (abstr-eqs->name abstr-eqs)))

           (define (abstr-eqs->relation abstr-eqs)
             `(relation ,(abstr-name->pred-name (abstr-eqs->name abstr-eqs))
                        ,(append (abstr-eqs->vars abstr-eqs) 
                                 (list (abstr-eqs->top-level-var abstr-eqs))
                                 (cond [(null? no-trees)
                                        (list 'TreeIDs)]
                                       [else '()]))

                        ;; if we have variables, run the body once,
                        ;; just to bind them
                        ,(append 

                           (cond [(< 0 (length (abstr-eqs->vars abstr-eqs)))
                                  (transform-body abstr-eqs #t)
                                  ]
                                 [else '()])

                           (list (cond [(null? no-trees)
                                        `(find_at_least_one
                                           TreeID
                                           ,(transform-body abstr-eqs #f)
                                           TreeIDs)]
                                       [else (transform-body abstr-eqs #f)])))
                        ))

           (map abstr-eqs->relation all-normalized-eqs))

         (define (relation->pl relation)
           (subexpr-walk (lambda (t) (cond [(eq? 'relation (car t))
                                            (let* ([name (cadr t)]
                                                   [vars (caddr t)]
                                                   [rhs (cadddr t)])
                                              (pl-clause (apply pl-relation (cons name vars))
                                                         (relation->pl rhs)))]
                                           [(eq? 'disj (car t))
                                            (apply pl-disj (map relation->pl (cdr t)))]
                                           [(eq? 'conj (car t))
                                            (apply pl-conj (map relation->pl (cdr t)))]
                                           [else t]))
                         relation))

                                            
                                            

         (define (program->pl prog . prefix-param)
           (define (prefix-anf prefix anf-abstrs)
             (let* ([abstr-names (map abstraction->name anf-abstrs)]
                    [new-names (map (lambda (name) (string->symbol (string-append prefix (symbol->string name))))
                                    abstr-names)]
                    [old->new (zip abstr-names new-names)]
                    [transform (lambda (t)
                                 (cond [(and (symbol? t) (assq t old->new)) (cadr (assq t old->new))]
                                       [else t]))])
               (sexp-walk transform anf-abstrs)))

           (let* ([prefix (cond [(null? prefix-param) ""]
                                [else (car prefix-param)])]
                  [no-trees '()]

                   ;; [db (begin (pretty-print "original prog:") (pretty-print prog))]
                  [cleaned-prog 
                                  (remove-unused-variables (dead-code-elimination prog))]

                   ;; [db (begin (pretty-print "program after removing unused variables:") (pretty-print cleaned-prog))]
                  [prog-anf (prefix-anf prefix (program->anf cleaned-prog))]
                  ;; [db (begin (print "A-normal form:") (pretty-print prog-anf))]
                  [abstr-names (map abstraction->name prog-anf)]
                  [abstr-eqs1 (map (lambda (abstr) (anf-abstr->equations abstr-names abstr)) prog-anf)]
                  ;; [db (begin (print "Intermediate equational form:") (pretty-print abstr-eqs1))]
                  [normalized-eqs (map distribute-choices (concatenate (map normalize-choices abstr-eqs1)))]
                  ;; [db (begin (print "After lifting out choices to top level:")
                             ;; (pretty-print normalized-eqs))]
                  [finalized (apply finalize-relations (cons normalized-eqs no-trees))]
                   ;; [db (begin (print "program:") (pretty-print prog))]
                   ;; [db (begin (print "Incorporating answer arguments and tree-building predicates:") (pretty-print finalized))]
                  [prolog-predicates (map relation->pl finalized)]
                  )
             prolog-predicates))

         (define (program->pl+features prog . prefix-param)
           (define (prefix-anf prefix anf-abstrs)
             (let* ([abstr-names (map abstraction->name anf-abstrs)]
                    [new-names (map (lambda (name) (string->symbol (string-append prefix (symbol->string name))))
                                    abstr-names)]
                    [old->new (zip abstr-names new-names)]
                    [transform (lambda (t)
                                 (cond [(and (symbol? t) (assq t old->new)) (cadr (assq t old->new))]
                                       [else t]))])
               (sexp-walk transform anf-abstrs)))

           (define (prefix-relations prefix finalized)
             (let* ([abstr-names (map cadr finalized)]
                    [new-names (map (lambda (name) (string->symbol (string-append prefix (symbol->string name))))
                                    abstr-names)]
                    [old->new (zip abstr-names new-names)]
                    [transform (lambda (t)
                                 (cond [(and (symbol? t) (assq t old->new)) (cadr (assq t old->new))]
                                       [else t]))])
               (sexp-walk transform finalized)))

           (let* ([prefix (cond [(null? prefix-param) ""]
                                [else (car prefix-param)])]
                  [no-trees '()]

                  [prog-anf (program->anf (remove-unused-variables (dead-code-elimination prog)))]
                  ;; [db (begin (print "A-normal form:")
                  ;; (pretty-print prog-anf))]
                  [abstr-eqs1 (map anf-abstr->equations prog-anf)]
                  ;; [db (begin (print "Intermediate equational form:")
                  ;; (pretty-print abstr-eqs1))]
                  [normalized-eqs (map distribute-choices (concatenate (map normalize-choices abstr-eqs1)))]
                  ;; [db (begin (print "After lifting out choices to top level:")
                             ;; (pretty-print normalized-eqs))]
                  [finalized (prefix-relations prefix
                               (apply finalize-relations-add-features 
                                    (list '(gauss gaussian) normalized-eqs)))]
                  [prolog-predicates (map relation->pl finalized)]
                  )
             prolog-predicates))


         (define (finalize-relations-add-features feature-names all-normalized-eqs . no-trees)

           (define abstr-names (map abstr-eqs->name all-normalized-eqs))

           (define (abstr-name->pred-name abstr-name)
             abstr-name)
             ;; (sym-append 'p abstr-name))

           (define (nondet? eqs) (eq? 'choose (car (abstr-eqs->eqs eqs))))

           (define (distributed-nondet-eqs->choices eqs)
             (cdr (abstr-eqs->eqs eqs)))


           (define (constructor? rhs) 
             (and (list? rhs) (not (contains? (car rhs) abstr-names))
                  (not (contains? (car rhs) feature-names))))
           (define (application? rhs)
             (and (list? rhs) (contains? (car rhs) abstr-names)))
           (define (feature? rhs)
             (and (list? rhs) (contains? (car rhs) feature-names)))
           (define (assert? rhs)
             (and (list? rhs) (contains? (car rhs) '(assertz assert asserta))))

           (define (body->relations pred-name choice-index num-choices pred-vars body no-tree-preds)
             (define my-subtrees '())

               (define (transform-eq eq)
                 (cond 
                   [(constructor? (eq->rhs eq)) eq]
                   [(application? (eq->rhs eq)) 
                    (let* ([renamed-rhs (cons (abstr-name->pred-name (car (eq->rhs eq)))
                                              (cdr (eq->rhs eq)))])
                      (append renamed-rhs (list (eq->lhs eq)) 
                              (cond [(null? no-trees) 
                                     (let* ([subtree-name (new-subtree-sym)])
                                       (begin (set! my-subtrees (cons subtree-name my-subtrees))
                                              (list subtree-name))
                                       )]
                                    [else '()])
                              ))]
                   [(feature? (eq->rhs eq)) 
                    ;; the problem is, this needs to be done _after_ the variables it refers to are ground. perhaps, we can have a postprocessing step after _transformed-eqs_ is called, so that the assertz's are always placed _after_
                    ;; alternatively, just place them last, right before the tree stuff.
                    (let* ([feature-args 
                             (cdr (eq->rhs eq))]
                           [lhs 
                             (eq->lhs eq)])
                      `(assertz (feature TreeID 
                                         ,`(,(car (eq->rhs eq))
                                             ,@(append feature-args 
                                                       (list lhs))))))]
                   [else eq]))

             (define (rearrange-asserts equations)
               (sort (lambda (x y)
                       (cond [(and (not (assert? x))
                                   (assert? y)) #t]
                             [else #f]))
                     equations))


             (let* ([transformed-eqs (rearrange-asserts (map transform-eq body))]
                    ;; [db (begin (print "Equations with asserts last:")
                               ;; (pretty-print transformed-eqs))]
                    )
               (rearrange-asserts
                 (append transformed-eqs
                       (cond [(and (not no-tree-preds) (null? no-trees))
                              (list `(= Tree 
                                        (tree ,pred-name ,choice-index ,num-choices
                                              (vars ,@pred-vars)
                                              ,@my-subtrees))
                                    '(add_if_not_present Tree TreeID))]
                             [else '()]

                             )))))

           (define (transform-one-alternative choice-index num-choices abstr-eqs body no-tree-preds)
             (let* ([relations (body->relations (abstr-name->pred-name (abstr-eqs->name abstr-eqs)) 
                                                choice-index 
                                                num-choices
                                                (abstr-eqs->vars abstr-eqs) 
                                                body
                                                no-tree-preds)])
               `(conj ,@relations)))

           (define (transform-body abstr-eqs no-tree-preds)
             (cond [(nondet? abstr-eqs)
                    (let* ([choices (distributed-nondet-eqs->choices abstr-eqs)]
                           [num-choices (length choices)]
                           [alternative->relation (lambda (idx alt) (transform-one-alternative idx
                                                                                               num-choices
                                                                                               abstr-eqs
                                                                                               alt
                                                                                               no-tree-preds))])
                    `(disj ,@(map alternative->relation (iota num-choices) (distributed-nondet-eqs->choices abstr-eqs))))]
                   [else (transform-one-alternative 0 1 abstr-eqs (abstr-eqs->eqs abstr-eqs) no-tree-preds)]))
           
           (define (is-top-level? abstr-eqs)
             (eq? 'TopLevel (abstr-eqs->name abstr-eqs)))

           (define (chop-off-asserts eqs)
             (define (chop-asserts eqs)
               (define (loop acc xs)
                 (cond [(null? xs) (reverse acc)]
                       [(assert? (car xs)) (reverse acc)]
                       [else (loop (cons (car xs) acc) (cdr xs))]))
               (loop '() eqs))
             (cond [(eq? 'disj (car eqs))
                    `(disj ,@(map chop-off-asserts (cdr eqs)))]
                   [else `(conj ,@(chop-asserts (cdr eqs)))]))

           (define (abstr-eqs->relation abstr-eqs)
             `(relation ,(abstr-name->pred-name (abstr-eqs->name abstr-eqs))
                        ,(append (abstr-eqs->vars abstr-eqs) 
                                 (list (abstr-eqs->top-level-var abstr-eqs))
                                 (cond [(null? no-trees)
                                        (list 'TreeIDs)]
                                       [else '()]))

                        ;; if we have variables, run the body once,
                        ;; just to bind them
                        ,(append 

                           (cond
                             [(not (is-top-level? abstr-eqs))
                              (let* ([repeated-body (chop-off-asserts (transform-body abstr-eqs #t))]
                                     [final (cond [(equal? '(conj) repeated-body) '()]
                                                  [else `(conj ,repeated-body)])])
                                final)]
                             [else '()])

                           (list (cond [(null? no-trees)
                                        `(find_at_least_one
                                           TreeID
                                           ,(transform-body abstr-eqs #f)
                                           TreeIDs)]
                                       [else (transform-body abstr-eqs #f)])))
                        ))

           (map abstr-eqs->relation all-normalized-eqs))

         (define (program->anf prog)
           (let* ([abstrs (program->abstractions prog)]
                  [body (program->body prog)]
                  [named-body `(abstraction TopLevel
                                            ()
                                            ,(third body))]
                  [abstr-names (cons 'choose (map abstraction->name abstrs))])
             (cons (anf-eval abstr-names named-body)
                   (map (curry anf-eval abstr-names) abstrs))))

         (define (remove-unused-variables prog)
           ;; the usage profile:
           ;; (<abstraction name> (list of #t, #f, #t if variable is unused, #f if not)).

           (define (has-unused-var? profile)
             (let* ([usages (cdr profile)])
               (disj usages)))

           (define (find-abstractions-vars-to-remove prog)

             (define abstrs (program->abstractions prog))

             (define (get-var-usage-profile abstr)
               (define (unused-var? var)
                 (let* ([result (deep-find-all (lambda (t) 
                                                 (or (equal? t var)
                                                     (equal? (car t) var)
                                                     (contains? var t))) (abstraction->pattern abstr))]
                        ;; [db (pretty-print "get-var-usage of")]
                        ;; [db (pretty-print var)]
                        ;; [db (pretty-print abstr)]
                        ;; [db (pretty-print result)]
                        )
                   (null? result)))
               (let* ([vars (abstraction->vars abstr)]
                      ;; [db (print "in get-var-usage-profile")]
                      [name (abstraction->name abstr)]
                      ;; [db (print (abstraction->pattern abstr))]
                      ;; [db (print vars)]
                      ;; [db (print name)]
                      ;; [db (print (map unused-var? vars))]
                      )
                 `(,name ,(map unused-var? vars))))
             (let* ([answer (map get-var-usage-profile abstrs)])
               (begin ;; (pretty-print answer)
                      answer)))

           (define (clean-abstrs-and-body profiles prog)

             (define (adjust-abstr profile abstr)
               (let*
                 (
                  [app-fx-name (car profile)]
                  [usage (cadr profile)]
                  [should-shorten? (lambda (app)
                                     (and (disj usage)
                                          (eq? app-fx-name (car app))
                                          (= (length usage) (length (cdr app)))))]
                  [positions-to-remove (list-idxs-where (lambda (x) x) usage) ]
                  [shorten-one-application (lambda (app)`(,(car app) ,@(list-remove-at-several positions-to-remove (cdr app))`(,(car app) ,@(list-remove-at-several positions-to-remove (cdr app)))))]
                  [new-pattern (subexpr-walk (lambda (t)
                                               (cond [(should-shorten? t)
                                                      (shorten-one-application t)]
                                                     [else t]))
                                             (abstraction->pattern abstr)) ]
                  [this-abstr? (eq? app-fx-name (abstraction->name abstr)) ]
                  [new-vars (cond [this-abstr? (list-remove-at-several positions-to-remove (abstraction->vars abstr))]
                                  [else (abstraction->vars abstr)])])

                 (begin
                   ;; (pretty-print abstr)
                   ;; (print "usage")
                   ;; (pretty-print usage)
                   ;; (print "positions-to-remove")
                   ;; (print positions-to-remove)
                   (make-named-abstraction
                     (abstraction->name abstr)
                     new-pattern
                     new-vars))))

             (define (remove-extra-app-args profiles sexpr)
               (define (shorten-app expr)
                 (let* ([profile-lookup (assq (car expr) profiles)])
                   (cond [(eq? #f profile-lookup) expr]
                         [else (let* ([usage (cadr profile-lookup)]
                                      [usage-args (zip usage (cdr expr))]
                                      [keep-arg? (lambda (use-arg) (equal? #f (car use-arg)))])
                                 `(,(car expr) ,@(map cadr (filter keep-arg? usage-args))))])))
               (subexpr-walk shorten-app sexpr))

             (define abstrs (program->abstractions prog))

             (define abstrs-with-new-headers
                 (map adjust-abstr profiles abstrs))

             (let* ([prog-with-new-abstr-headers 
                      (make-program
                        abstrs-with-new-headers
                        (program->body prog))]
                    [revised-abstrs
                      (map (lambda (abstr)
                             (make-named-abstraction
                               (abstraction->name abstr)
                               (remove-extra-app-args profiles (abstraction->pattern abstr))
                               (abstraction->vars abstr)))
                           (program->abstractions prog-with-new-abstr-headers))]
                    [revised-body
                      (remove-extra-app-args profiles (program->body prog-with-new-abstr-headers))])
               (make-program
                 revised-abstrs
                 revised-body)))

           (let* ([usage-profiles (find-abstractions-vars-to-remove prog)] ;; list of abstraction-name (#t #f #t) (depending on whether to remove the var or not)
                  [will-change (filter (lambda (profile) (disj (cadr profile)))
                                                           usage-profiles)]
                  )
                  
             (cond [(null? will-change) prog]
                   [else (let* ([cleaned-prog (clean-abstrs-and-body usage-profiles prog)])
                           (remove-unused-variables cleaned-prog))])))

         (define (dead-code-elimination prog)
           (let* ([abstrs (program->abstractions prog)]
                  [abstrs-without-local-dead-code
                    (map (lambda (abstr)
                           (make-named-abstraction
                             (abstraction->name abstr)
                             (local-remove-dead-code (abstraction->pattern abstr))
                             (abstraction->vars abstr)))
                         abstrs)])
             (make-program abstrs-without-local-dead-code (program->body prog))))

         ;; Removing dead code in the form of (lambda x. e y) where x never occurs in e
         ;; We assume no currying or other higher-order functions, every l-term is immediately applied
         (define (local-remove-dead-code pattern)
           ;; We work at the granularity of lambda-applications
           (define (l-app? e) (and (list? e) (list? (car e)) (eq? 'lambda (caar e))))
           (define l-app->vars cadar)
           (define l-app->body caddar)
           (define l-app->args cdr)
             (define (remove-local-var-decls t)
               (cond [(eq? 'lambda (car t)) 
                      `(lambda () ,(caddr t))]
                     [else t]))
           (define (loop expr)
             (cond [(l-app? expr)
                    (let* ([vars (l-app->vars expr)]
                           ;; [db (begin (print "expr: ") (pretty-print expr))]
                           ;; [db (begin (print "vars: ") (pretty-print vars))]
                           ;; [db (begin (print "body ") (pretty-print (l-app->body expr)))]
                           [without-decls (subexpr-walk remove-local-var-decls (l-app->body expr))]
                           ;; [db (begin (print "without-decls: ") (pretty-print without-decls))]
                           [used? (map (lambda (var)
                                         (reset (begin
                                                  (sexp-walk (lambda (t)
                                                               (cond [(eq? t var) (shift k #t)]
                                                                     [else t]))
                                                             without-decls)
                                                  (shift k #f))))
                                       vars)]
                           [dead-args? (conj (map not used?))])
                      (cond [dead-args? (begin
                                          ;; (print "args are dead, replacing with body")
                                          (l-app->body expr))]
                            [else (begin
                                    ;; (print "args not dead")
                                    ;; (print "@")
                                    ;; (pretty-print expr)
                                    ;; (pretty-print (l-app->args expr))
                                    `((lambda ,vars ,(loop (l-app->body expr)))
                                      ,@(l-app->args expr)))]))]
                   [else (begin
                           ;; (print "Not a lambda-application @ top level")
                           expr)]))
           (loop pattern))
)



         ;; TODO: perform lambda lifting on original program.
;; (define (lambda-lift prog)
;; 
;;   (define existing-abstr-names (map abstraction->name (program->abstractions prog)))
;; 
;;   (define (find-local-lterm-and-lift abstr)
;;     (let* ([bound-vars (abstraction->vars abstr)]
;;            [pattern (abstraction->pattern abstr)]
;;            )
;;       (reset
;;         (begin
;;           (subexpr-walk (lambda (t) (cond [(and (list? t) (eq? 'lambda (car t)))
;;                                            (shift k
;;       
;;   (define (lift-out-local-apps abstrs)
;;     (filter (lambda (res) (not (null? res))) 
;;             (map find-local-lterm-and-lift abstrs)))
;;   (let* ([abstrs (program->abstractions prog)]
;;          [new-abstrs (lift-out-local-apps abstrs)])
;;     (cond [(null? new-abstrs) prog]
;;           [else (lambda-lift
;;                   (make-program
;;                     (append new-abstrs abstrs)
;;                     (program->body prog)))])))
;; 
;; 
