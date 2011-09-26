(library (program-pl)
         (export program->pl
                 program->anf
                 anf-eval
                 anf-abstr->equations
                 ;; debug
                 normalized-abstr-eqs?
                 contains-choice?
                 choice-top-level?
                 var-sym?

                 finalize-relations
                 distribute-choices)
         (import (program)
                 (except (rnrs) string-hash string-ci-hash)
                 (_srfi :1)
                 (_srfi :69)
                 (util)
                 (printing)
                 (prolog-serialize))

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

         ;; probably the worst implementation of let flattening in the entire universe
         (define (anf-abstr->equations anf-abstr)
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
                                  ,(loop (abstraction->pattern anf-abstr))))))

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
                  [vars (lset-intersection equal? 
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


           (define (body->relations pred-name choice-index num-choices pred-vars body)
             (define my-subtrees '())
             (define (constructor? rhs) 
               (and (list? rhs) (not (contains? (car rhs) abstr-names))))
             (define (application? rhs)
               (and (list? rhs) (contains? (car rhs) abstr-names)))

             (define (transform-eq eq)
               (cond [(constructor? (eq->rhs eq)) eq]
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
                     [else eq]))

             (let* ([transformed-eqs (map transform-eq body)])
               (append transformed-eqs
                       (cond [(null? no-trees)
                              (list `(= Tree 
                                        (tree ,pred-name ,choice-index ,num-choices
                                              (vars ,@pred-vars)
                                              ,@my-subtrees))
                                    '(add_if_not_present Tree TreeID))]
                             [else '()]

                             ))))

           (define (transform-one-alternative choice-index num-choices abstr-eqs body)
             (let* ([relations (body->relations (abstr-name->pred-name (abstr-eqs->name abstr-eqs)) choice-index num-choices
                                                         (abstr-eqs->vars abstr-eqs) body)])
               `(conj ,@relations)))

           (define (transform-body abstr-eqs)
             (cond [(nondet? abstr-eqs)
                    (let* ([choices (distributed-nondet-eqs->choices abstr-eqs)]
                           [num-choices (length choices)]
                           [alternative->relation (lambda (idx alt) (transform-one-alternative idx
                                                                                               num-choices
                                                                                               abstr-eqs
                                                                                               alt))])
                    `(disj ,@(map alternative->relation (iota num-choices) (distributed-nondet-eqs->choices abstr-eqs))))]
                   [else (transform-one-alternative 0 1 abstr-eqs (abstr-eqs->eqs abstr-eqs))]))
           
           (define (is-top-level? abstr-eqs)
             (eq? 'TopLevel (abstr-eqs->name abstr-eqs)))

           (define (abstr-eqs->relation abstr-eqs)
             `(relation ,(abstr-name->pred-name (abstr-eqs->name abstr-eqs))
                        ,(append (abstr-eqs->vars abstr-eqs) 
                                 (list (abstr-eqs->top-level-var abstr-eqs))
                                 (cond [(null? no-trees)
                                        (cond [(is-top-level? abstr-eqs)
                                               (list 'TreeIDs)]
                                              [else (list 'TreeID)])]
                                       [else '()]))
                        ;; unfortunately, the combination of function
                        ;; arguments and recursive rules throws Prolog
                        ;; for a loop, at least the way I am writing
                        ;; the predicates. So, restrict
                        ;; find-at-least-one to the top-level.

                        ,(cond [(and (null? no-trees) (is-top-level? abstr-eqs))
                                `(find_at_least_one
                                   TreeID
                                   ,(transform-body abstr-eqs)
                                   TreeIDs)]
                               [else (transform-body abstr-eqs)])
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

                                            
                                            

         (define (program->pl prog . no-trees)
           (let* ([prog-anf (program->anf prog)]
                  [abstr-eqs1 (map anf-abstr->equations prog-anf)]
                  [normalized-eqs (map distribute-choices (concatenate (map normalize-choices abstr-eqs1)))]
                  [finalized (apply finalize-relations (cons normalized-eqs no-trees))]
                  [prolog-predicates (map relation->pl finalized)]
                  )
             prolog-predicates))


         (define (program->anf prog)
           (let* ([abstrs (program->abstractions prog)]
                  [body (program->body prog)]
                  [named-body `(abstraction TopLevel
                                            ()
                                            ,(third body))]
                  [abstr-names (cons 'chooose (map abstraction->name abstrs))])
             (cons (anf-eval abstr-names named-body)
                   (map (curry anf-eval abstr-names) abstrs))))
         )

