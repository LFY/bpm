(library (scene-graphs)
         (export reconstruct-dae
                 sample-grammar
                 sample-grammar+parameters
                 sample->sxml
                 sample->sxml-multiple
                 sample-multiple
                 output-scene-sampler
                 tie-parameters-to-choices
                 reconstitute)
         (import (except (rnrs) string-hash string-ci-hash)
                 (rnrs eval)
                 (only (scheme-tools) system)
                 (printing)
                 (_srfi :1)
                 (_srfi :69)
                 (util)
                 (node-constructors)
                 (program))


         (define (reconstruct-dae scene elements transforms . prefix)
           (define elt-table (alist->hash-table elements))
           (define tr-table (alist->hash-table transforms))

           (define (elt? e) (and (list? e) (eq? 'elem (car e))))
           (define (tr? e) (and (list? e) (eq? 'tr (car e))))

           (define (elt->sym e) (string->symbol (cadr e)))
           (define (tr->sym e) (string->symbol (cadr e)))

           (define tr->sub-elt caddr)

           (define (elt->def elt)
             (hash-table-ref elt-table (elt->sym elt)))
           (define (tr->def tr)
             (hash-table-ref tr-table (tr->sym tr)))

           (define counter 0)

           (define (gen-node-id e)
             (let* ([answer 
                      (string-append (cond [(null? prefix) ""]
                                           [else (car prefix)])
                                     (cond [(elt? e) (symbol->string (elt->sym e))]
                                           [(tr? e) (symbol->string (elt->sym (tr->sub-elt e)))])
                                     (number->string counter))])
               (begin (set! counter (+ 1 counter))
                      answer)))

           (define (loop e)
             (let* ([new-node-id (gen-node-id e)]
                    [attrs `(\x40; 
                              (id ,new-node-id)
                              (name ,new-node-id)
                              (type "NODE"))]
                    [maya-extra `(dae:extra
                                   (dae:technique (\x40; 
                                                (profile "OpenCOLLADAMaya"))
                                              (dae:originalMayaNodeId ,new-node-id)))])
               (cond [(elt? e) ;; b/c of top-level elt
                      `(dae:node ,attrs
                                 ,(elt->def e)
                                 ,@(map loop (cddr e))
                                 )]
                     [(tr? e)
                      `(dae:node ,attrs
                                 ,(tr->def e)
                                 ,(elt->def (tr->sub-elt e))
                                 ,@(map loop (cddr (tr->sub-elt e)))
                                 )]
                     [else e])))
           (loop scene))

         (define (sample-grammar program)
           (eval 

             `((let ()
                 ;; Sampling from the grammar

                 (define (scan f z xs)
                   (cond [(null? xs) `(,z)]
                         [else (let* ([res (f z (car xs))])
                                 (cons z (scan f res (cdr xs))))]))

                 (define (scan1 f xs)
                   (scan f (car xs) (cdr xs)))

                 ;; sampling from a discrete distribution

                 (define (rnd-select pvs)
                   (cond [(null? pvs) '()]
                         [else 
                           (letrec* ([smp (uniform-sample 0 1)]
                                     [pvs* (zip (scan1 + (map car pvs)) pvs)]
                                     [iterator (lambda (pvs)
                                                 (let* ([pv (car pvs)]
                                                        [p (car pv)]
                                                        [v (cadr pv)])
                                                   (cond [(< smp p) v]
                                                         [else (iterator (cdr pvs))])))])
                                    (iterator pvs*))]))

                 (define (mk-choice . vs)
                   (let* ([p (/ 1.0 (length vs))])
                     ((cadr (rnd-select (map (lambda (v) (list p v)) vs))))))

                 ;; lazy evaluation; don't want to eagerly evaluate choices because recursion.

                 (define-syntax process-choices
                   (syntax-rules ()
                                 [(process-choices) '()]
                                 [(process-choices e1 e2 ...) (cons (lambda () e1) (process-choices e2 ...))]
                                 ))

                 ;; we probably want to include parameters later

                 (define-syntax choose
                   (syntax-rules ()
                                 ((nondet-choice . xs) (apply mk-choice (process-choices . xs)))
                                 ))

                 (define-constr elem)
                 (define-constr tr)

                 ,(program->sexpr program)))
             (environment '(rnrs) '(util) '(node-constructors) '(_srfi :1))))

         ;; replace each (choose (F1) (F2)) with (choose (0.2 (lambda () (F1))) ... )
         
         (define (tie-parameters-to-choices grammar+params)
           (define (grammar->params grammar+params)
             (cadddr grammar+params))
           (define (my-grammar->nts grammar+params)
             (append (program->abstractions grammar+params)
                     (list `(abstraction TopLevel () ,(caddr (program->body grammar+params))))))
           (define nts-with-params
             (map (lambda (nt params)
                    (let* ([choices (cond [(eq? 'choose (car (abstraction->pattern nt))) 
                                           (cdr (abstraction->pattern nt))]
                                          [else (list (abstraction->pattern nt))])]
                           )
                      `(abstraction
                         ,(abstraction->name nt)
                         ()
                         (choose ,@(map (lambda (param thunk) `(list ,(exp param) ,thunk)) params (map (lambda (choice) `(lambda () ,choice)) choices))))))
                  (my-grammar->nts grammar+params) (grammar->params grammar+params)))
           `(program
              ,nts-with-params
              (lambda () (TopLevel))))

         (define (sample-grammar+parameters grammar+params)
           (let* ([prog (program->sexpr (tie-parameters-to-choices grammar+params))])
             (eval 

               `((let ()
                   ;; Sampling from the grammar

                   (define (scan f z xs)
                     (cond [(null? xs) `(,z)]
                           [else (let* ([res (f z (car xs))])
                                   (cons z (scan f res (cdr xs))))]))

                   (define (scan1 f xs)
                     (scan f (car xs) (cdr xs)))

                   ;; sampling from a discrete distribution

                   (define (rnd-select pvs)
                     (cond [(null? pvs) '()]
                           [else 
                             (letrec* ([smp (uniform-sample 0 1)]
                                       [pvs* (zip (scan1 + (map car pvs)) pvs)]
                                       [iterator (lambda (pvs)
                                                   (let* ([pv (car pvs)]
                                                          [p (car pv)]
                                                          [v (cadr pv)])
                                                     (cond [(< smp p) v]
                                                           [else (iterator (cdr pvs))])))])
                                      (iterator pvs*))]))

                   (define (mk-choice . pvs)
                     ((cadr (rnd-select pvs))))


                   ;; lazy evaluation; don't want to eagerly evaluate choices because recursion.

                   (define-syntax process-choices
                     (syntax-rules ()
                                   [(process-choices) '()]
                                   [(process-choices pv1 pv2 ...) (cons pv1 (process-choices pv2 ...))]
                                   ))

                   ;; we probably want to include parameters later

                   (define-syntax choose
                     (syntax-rules ()
                                   ((nondet-choice . pvs) (apply mk-choice (process-choices . pvs)))
                                   ))

                   (define-constr elem)
                   (define-constr tr)

                   ,prog))
               (environment '(rnrs) '(util) '(node-constructors) '(_srfi :1)))))

         (define (sample->sxml filename grammar elements transforms)
           (let* ([sample (sample-grammar+parameters grammar)])
             (begin
               (system (format "rm ~s" filename))
               (with-output-to-file filename (lambda () (pretty-print (list (reconstruct-dae sample elements transforms))))))))

         (define (sample->sxml-multiple k filename grammar elements transforms)
           (let* ([samples (map (lambda (i) (reconstruct-dae (sample-grammar grammar)
                                                             elements transforms
                                                             (string-append "model_"
                                                                            (number->string i)))) (iota k))])
             (begin
               (system (format "rm ~s" filename))
               (with-output-to-file filename (lambda () (pretty-print samples))))))

         (define (reconstitute original-file filename output-file)
           (begin
             (system "rm reconst.py")
             (with-output-to-file 
               "reconst.py"
               (lambda () (begin (print "from pyxml2prog import *")
                                 (print (format "rebuild_dae(~s, ~s, ~s)" original-file filename output-file)))))
             (system "python reconst.py")))

         (define (sample-multiple k scene-prefix original-file grammar elements transforms)
           (define scene-counter 0)
           (define (next-unused-name prefix)
             (if (file-exists? (string-append prefix (number->string scene-counter)))
               (begin (set! scene-counter (+ 1 scene-counter))
                      (next-unused-name prefix))
               (string-append prefix (number->string scene-counter))))
           (let* ((target-file
                    (next-unused-name scene-prefix))
                  (final-name (string-append target-file ".dae")))
             (begin
               (sample->sxml-multiple k target-file grammar elements transforms)
               (reconstitute original-file target-file final-name))))

         (define (output-scene-sampler original-file 
                                       filename 
                                       grammar 
                                       elements 
                                       transforms 
                                       scene-prefix)


           (let* ([bindings `(

                              (import (printing) (_srfi :1) (scene-graphs))

                              (define grammar (quote ,grammar))
                              (define elements (quote ,elements))
                              (define transforms (quote ,transforms))

                              (sample-multiple 1 ,scene-prefix ,original-file grammar elements transforms)
                              )])
             (with-output-to-file 
               filename 
               (lambda () (for-each pretty-print bindings)))))


         )
