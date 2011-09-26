(library (scene-graphs)
         (export reconstruct-dae
                 sample-grammar
                 sample->sxml)
         (import (except (rnrs) string-hash string-ci-hash)
                 (rnrs eval)
                 (only (scheme-tools) system)
                 (printing)
                 (_srfi :1)
                 (_srfi :69)
                 (util)
                 (node-constructors)
                 (program))


         (define (reconstruct-dae scene elements transforms)
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
                      (string-append (cond [(elt? e) (symbol->string (elt->sym e))]
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
                    [maya-extra `(extra
                                   (technique (\x40; 
                                                (profile "OpenCOLLADAMaya"))
                                              (originalMayaNodeId ,new-node-id)))])
               (cond [(elt? e) ;; b/c of top-level elt
                      `(dae:node ,attrs
                                 ,(elt->def e)
                                 ,@(map loop (cddr e))
                                 ,maya-extra)]
                     [(tr? e)
                      `(dae:node ,attrs
                                 ,(tr->def e)
                                 ,(elt->def (tr->sub-elt e))
                                 ,@(map loop (cddr (tr->sub-elt e)))
                                 ,maya-extra)]
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

         (define (sample->sxml filename grammar elements transforms)
           (let* ([sample (sample-grammar grammar)])
             (begin
               (system (format "rm ~s" filename))
               (with-output-to-file filename (lambda () (pretty-print (reconstruct-dae sample elements transforms)))))))

)
