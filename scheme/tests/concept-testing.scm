(library (concept-testing)
         (export run-multiple-data
                 replacement-fx
                 format-named-hypothesis
                 mk-prog-body
                 evaluate-multiple-programs

                 ;; concepts
                 concept1
                 concept2
                 
                 ;; experiment
                 run-experiment)
         (import (rnrs)
                 (prob-logic)
                 (query-learn)
                 (printing)
                 (program)
                 (dearguments)
                 (abstract)
                 (background-predicates)
                 (util)
                 (combinations)

                 (_srfi :1))

         (define (run-multiple-data data-names)
           (define (build-table next-col acc)
             (append acc next-col))
           (define (run-one-data data-name)
             (let* ([data (first data-name)]
                    [name (second data-name)]
                    [hyp-scores (feature-induction-n-iter 10 simple-soft-predicates data '() '())]
                    [learned-hyp (first hyp-scores)]
                    [first-col (first (second hyp-scores))]
                    [named-first-col (cons name first-col)]
                    [named-hyp (cons name (list learned-hyp))])
               (list named-hyp (list named-first-col))))
           (let* ([named-hyp-data-cols (map run-one-data data-names)]
                  [named-hyps (map first named-hyp-data-cols)]
                  [data-cols (map second named-hyp-data-cols)]
                  [max-len (apply max (map (lambda (x) (length (first x))) data-cols))]
                  [iter-col (list (cons 'Iteration (map (curry + 1) (iota max-len))))]
                  [all-cols (cons iter-col data-cols)])
             (list named-hyps (fold build-table '() all-cols))))

         (define replacement-fx (lambda (v) (cond [(or (eq? 'NULL v)
                                                       (eqv? -inf.0 v)) ""]
                                                  [else (format "~s" v)])))

         (define (format-named-hypothesis name-hyp)
           (let* ([name (first name-hyp)]
                  [hyp (second name-hyp)])
             (string-append "Name: " (str name) "\n" (format-hypothesis hyp)))) 

         (define (evaluate-one-program prog)
           (let* ([abstr (program->lookup-abstraction prog 'F1)]
                  ;; [db (print "attempting noisy recursion deargument on original: ~s" (noisy-arith-recursion-dearguments prog 'nofilter))]
                  [noisy-regular-transform (noisy-arith-deargument-transform prog abstr)]
                  [new-abstr (program->lookup-abstraction noisy-regular-transform 'F1)]
                  [noisy-regular-recursion-transform (noisy-arith-recursion-dearguments noisy-regular-transform 'nofilter)]
                  ;; [db (print "attempting noisy recursion deargument: ~s" noisy-regular-recursion-transform)]
                  )
             (cond [(null? noisy-regular-recursion-transform) noisy-regular-transform]
                   [else (first noisy-regular-recursion-transform)])))

         (define (evaluate-multiple-programs filename name-progs)
           (let* ([evaluated (map (lambda (xy) (list (first xy) (evaluate-one-program (second xy)))) name-progs)]
                  [display-one-named-program (lambda (name-prog)
                                               (begin
                                                 (print "~s after arithmetic deargumentation:" (first name-prog))
                                                 (pretty-print-program (second name-prog))
                                                 (print "")))])
             (with-output-to-file filename
                                  (lambda () (for-each display-one-named-program evaluated)))))


         (define (mk-prog-body p) (list 'lambda '() p))

         ;; we'd like to generate individual function applications of F1 that describe some concept,
         ;; along with the initial form of the function.

         (define (noisy-bop op x y mean var)
           (op x y (sample-gaussian mean var)))

         (define (noisy* x y var) (noisy-bop * x y 1 var))
         (define (noisy+ x y var) (noisy-bop + x y 0 var))
         (define (noisy- x y var) (noisy-bop - x y 0 var))

         ;; random generate argument matrix,
         ;; then generate a set of nonrec. applications
         (define (mk-nonrec-body row-gen abstr-name count)
           (let* ([mk-one-app (lambda () `(,abstr-name ,@(row-gen)))])
             `(node (data) ,@(map mk-one-app (iota count)))))

         (define (mk-prog-data call-gen count)
           `(node (data) ,@(map (lambda (i) (call-gen)) (iota count))))

         (define (nonrec-gen abstr-name row-gen)
           `(,abstr-name ,@(row-gen)))

         (define (rec-gen abstr-name init-row-gen row->nextrow end len)
           (define (loop current-row n)
             (cond [(= 0 n) (end current-row)]
                   [else `(,abstr-name ,@current-row
                                       ,(loop (row->nextrow current-row) (- n 1)))]))
           (loop (init-row-gen) len))

         (define (init-range) (uniform-sample -20 20))


         (define (mk-var-sym i)
           (string->symbol (format "V~d" i)))

         (define (mk-attr-sym i)
           (string->symbol (format "A~d" i)))

         (define (mk-nonrec-function name num-vars)
           (let* ([vars (map (lambda (i) (mk-var-sym i)) (iota num-vars))]
                  [pattern `(node (data ,@(map (lambda (i) `(,(mk-attr-sym i) ,(mk-var-sym i))) (iota num-vars))))])
             (make-named-abstraction name pattern vars)))

         (define (mk-rec-function name num-vars)
           (let* ([vars (map (lambda (i) (mk-var-sym i)) (iota num-vars))]
                  [pattern `(node (data ,@(map (lambda (i) `(,(mk-attr-sym i) ,(mk-var-sym i))) (iota num-vars)))
                                  ,(mk-var-sym num-vars))])
             (make-named-abstraction name pattern vars)))

         (define (concept1 count var)
           (define (row-gen)
             (let* ([x0 (init-range)]
                    [x1 (noisy+ x0 1 var)]
                    [x2 (noisy+ x1 1 var)]
                    [x3 (noisy+ x2 1 var)])
               (list x0 x1 x2 x3)))
           (make-program
             (list (mk-nonrec-function 'F1 4))
             (mk-prog-body (mk-prog-data (lambda () (nonrec-gen 'F1 row-gen)) count))))

         (define concept1-ground-truth
           (make-named-abstraction 'F1 '(data (A0 V0)
                                              (A1 (+ V0 1))
                                              (A2 (+ V0 2))
                                              (A3 (+ V0 3))) '(V0)))


         (define (concept2 count var)
           (define len 10)
           (define (init-row-gen)
             (let* ([x0 (init-range)]
                    [x1 (noisy+ x0 2 var)])
               (list x0 x1)))
           (define (row->nextrow row)
             (let* ([x0 (noisy+ (first row) 1 var)]
                    [x1 (noisy+ x0 2 var)])
               (list x0 x1)))
           (define (row->end row)
             (let* ([x0 (noisy+ (first row) 1 var)]
                    [x1 (noisy+ x0 2 var)])
             `(node (data (A0 ,x0)
                          (A1 ,x1)))))

           (make-program
             (list (mk-rec-function 'F1 3))
             (mk-prog-body (mk-prog-data (lambda () (rec-gen 'F1 init-row-gen row->nextrow row->end len)) count))))

         (define concept2-ground-truth
           (make-named-abstraction 'F1 '(let* ([V1 (if (flip)
                                                     (F1 (+ V0 1))
                                                     (node (data A0 0)))])
                                          (node (data (A0 V0) (A1 (+ V0 2))) V1))
                                    '(V0)))

         (define (concept3 count var)
           (define len 5)
           (define (init-row-gen)
             (list (init-range)))
           (define (row->nextrow row)
             (list (noisy+ (first row) 1 var)))
           (define (row->end row)
             `(node (data (A0 ,(noisy+ (first row) 1 var)))))

           (make-program
             (list (mk-rec-function 'F1 2))
             (mk-prog-body (mk-prog-data (lambda () (rec-gen 'F1 init-row-gen row->nextrow row->end len)) count))))

         (define concept3-ground-truth
           (make-named-abstraction 'F1 '(let* ([V1 (if (flip)
                                                     (F1 (+ V0 1))
                                                     (node (data A0 0)))])
                                          (node (data (A0 V0)) V1))
                                    '(V0)))

         (define (concept4 count var)
           (define len 5)
           (define (init-row-gen)
             (list (init-range)))
           (define (row->nextrow row)
             (list (noisy* (first row) -1 var)))
           (define (row->end row)
             `(node (data (A0 ,(noisy* (first row) -1 var)))))

           (make-program
             (list (mk-rec-function 'F1 2))
             (mk-prog-body (mk-prog-data (lambda () (rec-gen 'F1 init-row-gen row->nextrow row->end len)) count))))

         (define concept4-ground-truth
           (make-named-abstraction 'F1 '(let* ([V1 (if (flip)
                                                     (F1 (* V0 -1))
                                                     (node (data A0 0)))])
                                          (node (data (A0 V0)) V1))
                                    '(V0)))

         (define (concept5 count var)
           (define (row-gen)
             (let* ([x0 (init-range)]
                    [x1 (noisy* x0 -1 var)]
                    [x2 (noisy+ x0 1 var)]
                    [x3 (noisy* x2 -1 var)])
               (list x0 x1 x2 x3)))
           (make-program
             (list (mk-nonrec-function 'F1 4))
             (mk-prog-body (mk-prog-data (lambda () (nonrec-gen 'F1 row-gen)) count))))

         (define concept5-ground-truth
           (make-named-abstraction 'F1 '(node (data (A0 V0)
                                                    (A1 (- V0))
                                                    (A2 (+ V0 1))
                                                    (A3 (- (+ V0 1)))))
                                   '(V0)))

         (define test-concepts (list concept1
                                     concept2
                                     concept3
                                     concept4
                                     concept5))

         (define concept-truth (list
                                 (list concept1 concept1-ground-truth)
                                 (list concept2 concept2-ground-truth)
                                 (list concept3 concept3-ground-truth)
                                 (list concept4 concept4-ground-truth)
                                 (list concept5 concept5-ground-truth)))


         (define (run-experiment count vars)
           (define (run-one-data concept count var)
             (evaluate-one-program (concept count var)))
           (define (display-result ground-truth result count var)
             (begin (print "testing concept with ~s examples and variance ~s:" count var)
                    (pretty-print ground-truth)
                    (print "learned function:")
                    (pretty-print (program->lookup-abstraction result 'F1))
                    ;; (pretty-print result)
                    (print "")))
           (let* ([all-experiments (cartesian-product concept-truth vars)])
             (for-each (lambda (concept-truth-var)
                         (let* ([concept-truth (first concept-truth-var)]
                                [var (second concept-truth-var)]
                                [concept (first concept-truth)]
                                [ground-truth (second concept-truth)]
                                [result (run-one-data concept count var)])
                           (display-result ground-truth result count var)))
                       all-experiments)))

         )
