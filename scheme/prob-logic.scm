(library (prob-logic)
         (export learn-predicates
                 learn-predicates-keep-vars
                 learn-soft-predicates

                 feature-induction-gen

                 feature-induction-n-iter
                 feature-induction-threshold
                 feature-induction-full

                 sample-symbol
                 predicate-symbol
                 print-hypothesis-score
                 format-hypothesis
                 pretty-format-hypothesis

                 ; debug
                 get-all-soft-facts
                 find-common-soft-facts
                 facts-to-predicate
                 soft-facts-to-predicate

                 ; substitutions
                 derive-equalities
                 derive-addition-equations
                 learn-facts
                 simplify-facts
                 generate-substitutions

                 find-best-representatives

                 pred->facts

                 ; feature induction debug
                 induce-one-step
                 get-refinements
                 idx->vals
                 data-hyp->log-likelihood
                 argmax
                 
                 )
         (import (except (rnrs) string-hash string-ci-hash)
                 (_srfi :1)
                 (_srfi :69)

                 (dearguments)
                 (program)
                 (sym)

                 (background-predicates)

                 (util)
                 (combinations)
                 (graph)
                 (printing)

                 )



         ; Feature induction algorithm
         ; format of hypotheses:
         ; (list
         ;   (list pred idx))

         ; threshold: after the likelihood of the data falls below this threshold, the algorithm stops

         ; this is a breadth-first search (for now) (possible modifications:
         ; bounded-depth search, iterative deepening)

         ; what happens if there are no refinements...

         (define (induce-one-step background data current-hypothesis)
           (let* ([refinements (get-refinements background data current-hypothesis)]
                  [refinement-scores (zip refinements
                                          (map (curry data-hyp->log-likelihood data) refinements))]
                  [best-refinement-score (argmax second refinement-scores)]
                  ;[best-hyp (first best-refinement-score)]
                  ;[db (print "best hypothesis: ~s" (pretty-format-hypothesis best-hyp))]
                  )
             best-refinement-score))

         ; we'd like to factor out the stopping criterion from the rest of the
         ; function, and be able to mix and match them. Iteratee?
         
         ; more generic version of feature induction that takes an iteration
         ; continuation controlling the stop condition and building up
         ; debugging information

         ; iteration-fx returns 2 things: whether or not to stop, and some internal state.

         (define (feature-induction-gen background data current-hypothesis iteration-fx curr-state)
           (let* ([new-hyp-score (induce-one-step background data current-hypothesis)]
                  [shouldstop-state (iteration-fx current-hypothesis new-hyp-score curr-state)]
                  [shouldstop (first shouldstop-state)]
                  [next-state (second shouldstop-state)])
             (cond [shouldstop (list current-hypothesis next-state)]
                   [else (feature-induction-gen background data (first new-hyp-score) iteration-fx next-state)])))

         (define (feature-induction-threshold threshold background data current-hypothesis scores)
           (feature-induction-gen background data current-hypothesis (mk-threshold-stop-with-score-building threshold) scores))

         (define (feature-induction-n-iter n background data current-hypothesis scores)
           (feature-induction-gen background data current-hypothesis (mk-n-iter-stop-with-score-building n) scores))

         (define (feature-induction-full background data current-hypothesis scores)
           (feature-induction-gen background data current-hypothesis (mk-null-stop-with-score-building) scores))

         (define (mk-threshold-stop-with-score-building threshold)
           (define curr-col '())

           (define (add-results scores)
             (append scores (list curr-col)))

           (define (add-one-score! score)
             (set! curr-col (append curr-col (list score))))

           (define (iteration-fx current-hypothesis new-hyp-score scores)
             (cond [(null? new-hyp-score) (list #t (add-results scores))]
                   [(> threshold (second new-hyp-score)) (list #t (add-results scores))]
                   [else (begin
                           (add-one-score! (second new-hyp-score))
                           (list #f scores))]))
           iteration-fx)

         (define (mk-n-iter-stop-with-score-building n)
           (define counter 0)

           (define curr-col '())

           (define (add-results scores)
             (append scores (list curr-col)))

           (define (add-one-score! score)
             (set! curr-col (append curr-col (list score))))

           (define (iteration-fx current-hypothesis new-hyp-score scores)
             (cond [(null? new-hyp-score) (list #t (add-results scores))] 
                   [(< counter n) (begin
                                    (add-one-score! (second new-hyp-score))
                                    (set! counter (+ 1 counter))
                                    (list #f scores))]
                   [else (list #t (begin
                                    (add-one-score! (second new-hyp-score))
                                    (add-results scores)))]))
           iteration-fx)

         (define (mk-null-stop-with-score-building)

           (define curr-col '())

           (define (add-results scores)
             (append scores (list curr-col)))

           (define (add-one-score! score)
             (set! curr-col (append curr-col (list score))))

           (define (iteration-fx current-hypothesis new-hyp-score scores)
             (cond [(null? new-hyp-score) (list #t (add-results scores))]
                   [else (begin
                           (add-one-score! (second new-hyp-score))
                           (list #f scores))]))
           iteration-fx)


         ; (define (feature-induction threshold background data current-hypothesis)
         ;   (let* ([new-hyp-score (induce-one-step background data current-hypothesis)])
         ;     (cond [(null? new-hyp-score) (begin (print "Could not find valid refinement of current hypothesis. Stopping.")
         ;                                         current-hypothesis)] ; couldn't find any legal refinements.
         ;           [(> threshold (second new-hyp-score)) (begin (print "No refinement is above threshold. Stopping.")
         ;                                                        current-hypothesis)] ; score of the new hypothesis is less than threshold.
         ;           [else (begin (print "Continuing with new hypothesis:")
         ;                        (print-hypothesis-score new-hyp-score)
         ;                        (feature-induction threshold background data (first new-hyp-score)))]))) ; keep going otherwise

         ; (define (feature-induction-n-iter n background data current-hypothesis)
         ;   (let* ([new-hyp-score (induce-one-step background data current-hypothesis)])
         ;     (cond [(null? new-hyp-score) (begin (print "Could not find valid refinement (~d iterations left). Stopping with current hypothesis." (+ n 1))

         ;                                         current-hypothesis)]
         ;           [(eq? 0 n) (begin (print "No more iterations. The resulting hypothesis:")
         ;                             (print-hypothesis-score new-hyp-score))]
         ;           [else (begin (print "Iterations left: ~d:" (+ n 1))
         ;                        (print-hypothesis-score new-hyp-score)
         ;                        (feature-induction-n-iter (- n 1) background data (first new-hyp-score)))])))



         ; Possible refinements of the current hypothesis:
         ; 1. Add an atomic predicate from the background knowledge that has not been used before on the same variables.
         ; 2. By "not been used before on the same variables", I mean that
         ; 3. Another restriction is that all arguments to each new background predicate we introduce are different and that we do not generate frivolous predicates like equals(X, Y) && equals(Y, X).
         ; 4. (For now) No local variables allowed in the hypothesis.

         ; Possible improvements:
         ; 1. Generate predicate applications lazily.
         ; 2. Do not consider logically redundant predicates (like Greater(X, Z) when Greater(X, Y) and Greater (Y, Z) already exist).
         ; 3. Add local variables and increase depth of search to be able to use them.
         ; 4. It seems that the likelihood function goes to several different "plateaus" depending on how unreasonble the new refinement is. Possible to decide on a stopping threshold dynamically?
         ; 5. Consider search over parameterized predicates (same local variables basically)
         (define (get-refinements background data current-hypothesis)
           ; We need generators of predicates and indexings
           ; We take them from the data itself.

           (define (generate-predicate-applications)
             (let* ([all-vars (iota (length (first data)))]
                    [all-applications (lambda (b)
                                        (cond [(commutative? b) (select-k-comm (pred->arity b) all-vars)]
                                              [else (select-k (pred->arity b) all-vars)]))])
               (concatenate (map (lambda (b)
                                   (map (lambda (app)
                                          (list b app)) (all-applications b)))
                                 background))))

           (define (generate-2-compositions)
             (let* ([commutative-binary-predicates (filter commutative? (filter (lambda (p) (eq? 2 (pred->arity p))) background))]
                    ;[c (print commutative-binary-predicates)]
                    [comm-pred-pairs (cartesian-product commutative-binary-predicates commutative-binary-predicates)]
                    ;[c (print comm-pred-pairs)]
                    [num-vars (max (length (all-idxs current-hypothesis)) (length (first data)))]
                    [all-bound-vars (iota (length (first data)))]
                    [all-vars (iota num-vars)]
                    [new-var num-vars]
                    [all-diff-pairs (select-k 2 all-bound-vars)]
                    [pairs-to-apps (map (lambda (xy) 
                                          (list (list (first xy) new-var)
                                                (list (second xy) new-var))) all-diff-pairs)]
                    [pred-apps-for-pair (lambda (p1p2)
                                          (let* ([p1 (first p1p2)]
                                                 [p2 (second p1p2)])
                                            (map (lambda (idx1idx2)
                                                   (let* ([idx1 (first idx1idx2)]
                                                          [idx2 (second idx1idx2)])
                                                     (list (list p1 idx1)
                                                           (list p2 idx2)))) pairs-to-apps)))]
                    [result (concatenate (map pred-apps-for-pair comm-pred-pairs))])
               result))

           (define (already-used? pred-idx)
             (contains? pred-idx current-hypothesis))


           (define (legal-app? pred-idx)
             (and (not (already-used? pred-idx))
                  (can-apply? (first pred-idx) 
                              (idx->vals (first data) (second pred-idx)))))

           (let* ([new-applications (generate-predicate-applications)]
                  ;[new-compositions (generate-2-compositions)]
                  ;[c (print "new compositions: ~s" new-compositions)]
                  ;[legal-refinements (filter legal-app? (append new-applications new-compositions))])
                  [legal-application-refinements (filter legal-app? new-applications)])
                  ;[legal-composition-refinements (filter (lambda (p1p2) (and (legal-app? (first p1p2))
                   ;                                                          (legal-app? (second p1p2)))) new-compositions)])
                  ;[legal-refinements (filter legal-app? new-applications)])
             (append (map (lambda (r) (cons r current-hypothesis)) legal-application-refinements)
                     )))
                     ;(map (lambda (r) (append r current-hypothesis)) legal-composition-refinements))))

         (define (all-idxs hyp)
           (delete-duplicates (concatenate (map second hyp))))

         (define (idx->vals row idx)
           ; turns up 'H if it's not in row
           (define (get-val i)
             (cond [(< i (length row)) (list-ref row i)]
                   [else 'H]))
           (map get-val idx))

         (define (idx->vals->unify row p idx)
           (let* ([intermediate (idx->vals row idx)])
             (one-step-unify p intermediate)))

         (define (get-unified-sols res)
           (filter list? res))

         ; next: score each composition for its likelihood. this will require
         ; us to track local variables encountered in the hypothesis, unify the
         ; predicates in which they appear to get possible answers, and to
         ; apply soft-equality to the unified values (for each local variable).
         
         (define (get-local-vars data current-hypothesis)
           (let* ([num-global-vars (length (first data))]
                  [global-vars (iota num-global-vars)]
                  [local-vars (lset-difference eq? (all-idxs current-hypothesis) global-vars)])
             ;(print "local vars: ~s" local-vars)
             local-vars))

         (define (get-var-apps current-hypothesis var)
           (let* ([is-applied? (lambda (p-idx) (contains? var (second p-idx)))])
             (filter is-applied? current-hypothesis)))

         (define (replace-hole args val)
           (map (lambda (x) (cond [(eq? 'H x) val]
                                  [else x])) args))

         (define (comp-pred-idx pidx1 pidx2)
           (and (equal? (second pidx1) (second pidx2))
                (equal? (pred->name (first pidx1)) (pred->name (first pidx2)))))


         (define (data-hyp->log-likelihood data hyp)

           (define local-vars (get-local-vars data hyp))
           (define local-var-apps (map (curry get-var-apps hyp) local-vars))
           (define bound-var-apps 
             (begin 
               ;(print "hypothesis: ~s" hyp)
               ;(print "local var apps: ~s" local-var-apps)
               ;(print "result: ~s" (lset-difference eq? hyp (concatenate local-var-apps)))
               (lset-difference eq? hyp (concatenate local-var-apps))))

           (define (single-log-likelihood row)

             (define (get-unify-results pred-idx)
               (let* ([pred (first pred-idx)]
                      ;[c (print "pred: ~s" (pred->name pred))]
                      [idx (second pred-idx)]
                      ;[c (print "idx: ~s" idx)]
                      [args-with-hole (idx->vals row idx)]
                      [unify-result (one-step-unify pred args-with-hole)])
                 unify-result))

             (define (apply-one-pred-with-free-var pred-idx)
               (let* ([pred (first pred-idx)]
                      [idx (second pred-idx)]
                      [unify-result (get-unify-results pred-idx)]
                      [val-with-unify (apply pred (replace-hole (idx->vals row idx) (first unify-result)))])
                 val-with-unify))

             (define (score-unifications pred-idxs)
               (let* ([unify-results (map get-unify-results pred-idxs)]
                      [unify-pairs (apply cartesian-product unify-results)]
                      [all-soft-eq (map (curry apply soft-eq?) unify-pairs)]
                      [maximum-score (apply max all-soft-eq)])
                 maximum-score))

             (define (process-local-var-app-set pred-idxs)
               (begin
                 ;(print "local var apps: ~s" local-var-apps)
                 (apply + (map log (cons (score-unifications pred-idxs) (map apply-one-pred-with-free-var pred-idxs))))))
             
             (define (apply-one-pred pred-idx)
               (let* ([pred (first pred-idx)]
                      [idx (second pred-idx)]
                      [val (log (apply pred (idx->vals row idx)))])
                 val))
             (let* ([result (apply + (append (map apply-one-pred bound-var-apps)
                                  (map process-local-var-app-set local-var-apps)))])
               result))
           (apply + (map single-log-likelihood data)))

         (define (argmax f xs)
           (cond [(null? xs) '()]
                 [else 
                   (first (sort (lambda (x y)
                                  (> (f x) (f y))) xs))]))

         (define (pretty-format-hypothesis hyp)
           (define all-idxs
             (delete-duplicates (concatenate (map second hyp))))
           (define (format-one-pred-app pred-idx)
             (let* ([pred (first pred-idx)]
                    [idx (second pred-idx)])
               (string-append "(" (pred->name pred) " " (delimit-format " " idx) ")")))
           (string-append "(p " (delimit-format " " all-idxs) ") <- "
                          (delimit-with-formatter format-one-pred-app " " (reverse hyp))))


         (define (format-hypothesis hyp)
           (define all-idxs
             (delete-duplicates (concatenate (map second hyp))))
           (define (format-one-pred-app pred-idx)
             (let* ([pred (first pred-idx)]
                    [idx (second pred-idx)])
               (string-append (pred->name pred) " " (delimit-format " " idx))))
                          (delimit-with-formatter format-one-pred-app "\n" (reverse hyp)))
         (define (print-hypothesis-score hyp-score)
           (define (print-one-pred-app pred-idx)
             (let* ([pred (first pred-idx)]
                    [idx (second pred-idx)])
               (begin
                 (display-all (pred->name pred) " " idx "\n"))))
           (begin
             (display-all "Hypothesis:\n")
             (for-each print-one-pred-app (first hyp-score))
             (display-all "Score: " (second hyp-score) "\n")))


         ; Other algorithms
         
         (define (learn-soft-predicates prog abstr)
           (let* ([mat (arg-matrix prog abstr)]
                  [rows (apply zip mat)]
                  [row-soft-facts (map get-all-soft-facts rows)]
                  [remaining (find-common-soft-facts row-soft-facts)])
             (soft-facts-to-predicate remaining)))

         ; For now, just calc the sum and compare with 0.8 * max possible score
         (define (soft-facts-to-predicate facts)
           ; We want to take each place (1 2 3 ..) to a new symbol
           ; A hash table keeps track of them.

           (define place-to-vars (make-hash-table eqv?))
           (define (to-var place)
             (if (hash-table-exists? place-to-vars place)
               (hash-table-ref place-to-vars place)
               (begin
                 (hash-table-set! place-to-vars place (sym (sample-symbol)))
                 (hash-table-ref place-to-vars place))))

           ; Converts one fact to several applications
           (define (fact-to-pred-apps fact)
             (let* ([pred (first fact)]
                    [vars (second fact)]
                    [appvars (map (curry map to-var) vars)]
                    [mk-one-app (lambda (vs) `(,pred ,@vs))])
               (map mk-one-app appvars)))

           (let* ([res-sym (sym (predicate-symbol))]
                  [all-apps (concatenate (map fact-to-pred-apps facts))]
                  [res-pattern 
                    `(> (+ ,@all-apps)
                        (* ,0.8 ,(length all-apps)))]
                  [res-vars (hash-table-values place-to-vars)])
             (make-named-abstraction 
               res-sym
               res-pattern
               res-vars)))

         (define (find-common-soft-facts row-soft-facts)
           (let* ([index-pred-score-groups (group-by-indexing (concatenate row-soft-facts))]
                  [remaining-groups (filter has-consistent-cluster index-pred-score-groups)]
                  [representative-idx-pred-scores (map first remaining-groups)]
                  [representative-facts (map (lambda (ips) (list (second ips) (list (first ips)))) representative-idx-pred-scores)])
             representative-facts))

         ; From (pred (idx-score)) to (idx (pred-score))
         (define (group-by-indexing soft-facts)
           (define (soft-fact->index-pred-scores fact)
             (let* ([pred (first fact)]
                    [idx-scores (second fact)]
                    [idx-score->idx-pred-score
                      (lambda (idxscore)
                        (list (first idxscore) pred (second idxscore)))])
               (map idx-score->idx-pred-score idx-scores)))
           (let* ([all-index-pred-scores (concatenate (map soft-fact->index-pred-scores soft-facts))]
                  [grouped-by-index (group-by equal? first all-index-pred-scores)]
                  [grouped-by-index-pred (concatenate (map (curry group-by
                                                                  equal? 
                                                                  (lambda (x) (pred->name (second x))))
                                                           grouped-by-index))])
             grouped-by-index-pred))

         ; (idx-pred-scores): #t if there is a strong cluster, #f otherwise
         ; Other schemes may be better. this one just calculates the avg score of a predicate
         ; and compares it to the max
         (define (has-consistent-cluster idx-pred-scores)
           (let* ([sum-scores (apply + (map third idx-pred-scores))])
             (< (- (length idx-pred-scores) sum-scores) (* 0.2 (length idx-pred-scores)))))

         ; Each soft fact is now: (predicate (list list-of-indices score))
         (define (get-all-soft-facts example)
           (define (generate-soft-fact pred)
             (define (apply-pred-ex ivs)
               (apply pred (map second ivs)))
             (define (well-typed-candidate ivs)
               (can-apply? pred (map second ivs)))
             (let* ([k (pred->arity pred)]
                    [ex (zip (iota (length example)) example)]
                    [candidates (if (commutative? pred)
                                  (select-k-comm k ex)
                                  (select-k k ex))]
                    [well-typed-candidates (filter well-typed-candidate candidates)]
                    [candidate-scores (map apply-pred-ex well-typed-candidates)]
                    [indexing-scores (zip (map (curry map first) well-typed-candidates) candidate-scores)])
               (list pred indexing-scores)))
           (map generate-soft-fact simple-soft-predicates))


         ; 2. Domain-specific predicates (distance, straightness, etc)
         ;
         ;

         ; 0. Obtain call matrix

         ; 1. For each background predicate, go over each row in the argxcall matrix

         (define (learn-predicates prog abstr)
           (let* ([mat (arg-matrix prog abstr)] ; column-major
                  [rows (apply zip mat)] ; row-major
                  [row-preds (map get-all-facts rows)]
                  [remaining (find-common-facts row-preds)]
                  [db (begin
                        (print "resulting facts: ~s" remaining))])
             (facts-to-predicate remaining)))

         (define (learn-predicates-keep-vars prog abstr)
           (let* ([vars (abstraction->vars abstr)]
                  [mat (arg-matrix prog abstr)]
                  [rows (apply zip mat)]
                  [row-preds (map (curry get-all-facts-with-vars vars) rows)]
                  [remaining (find-common-facts row-preds)]
                  [db (begin
                        (print "resulting facts: ~s" remaining))])
             (facts-to-predicate-with-vars vars remaining)))

         (define (learn-facts prog abstr)
           (let* ([mat (arg-matrix prog abstr)]
                  [rows (apply zip mat)]
                  [row-preds (map get-all-facts rows)]
                  [remaining (find-common-facts row-preds)])
             remaining))


         ; The format of a fact: (predicate scopes)
         ; scopes: list of lists of indices representing where predicate applies
         (define (get-all-facts example)
           (define (apply-pred-ex pred ivs)
             (let* ([all-args (map second ivs)]
                    [well-typed (can-apply? pred all-args)])
               (if well-typed
                 (apply pred all-args)
                 #f)))
           (define (generate-fact pred)
             (let* ([k (pred->arity pred)]
                    [ex (zip (iota (length example)) example)]
                    [candidates (if (commutative? pred)
                                  (select-k-comm k ex)
                                  (select-k k ex))]
                    [consistent-candidates (filter (curry apply-pred-ex pred) candidates)])
               (list pred (map (curry map first) consistent-candidates))))
           (let* ([result (filter 
                            (lambda (f) (not (null? (second f)))) 
                            (map generate-fact simple-hard-predicates))]
                  )
             result))

         (define (get-all-facts-with-vars vars example)
           ;; critical part: mapping vars to examples. assume col order = variable order

           ;; (define (var->val var)
           ;;   (list-ref example (second (assq (zip vars (iota (length vars)))))))

           ;; (define (vars->vals vars) (map var->val vars))

           (define (apply-pred-ex pred ivs)
             (let* ([all-args (map second ivs)]
                    [well-typed (can-apply? pred all-args)])
               (if well-typed
                 (apply pred all-args)
                 #f)))
           (define (generate-fact pred)
             (let* ([k (pred->arity pred)]
                    [ex (zip vars example)]
                    [candidates (if (commutative? pred)
                                  (select-k-comm k ex)
                                  (select-k k ex))]
                    [consistent-candidates (filter (curry apply-pred-ex pred) candidates)])
               (list pred (map (curry map first) consistent-candidates))))
           (let* ([result (filter 
                            (lambda (f) (not (null? (second f)))) 
                            (map generate-fact simple-hard-predicates))]
                  )
             result))


         (define (find-common-facts fact-sets)
           (define (in-all-sets fact)
             (conj (map (lambda (fset) (contains? fact fset)) fact-sets)))
           (let* ([all-facts (delete-duplicates (concatenate fact-sets))]
                  [remaining (filter in-all-sets all-facts)])
             remaining))

         ; Converts the predicate to an abstraction

         ; going the other way. assumes pred is a conjunction
         (define (pred->facts pred)
           (define pred-app->pred car)
           (define pred-app->vars cdr)

           (define var-counter 0)

           (define var->place (make-hash-table eqv?))

           (define (to-place var)
             (if (hash-table-exists? var->place var)
               (hash-table-ref var->place var)
               (begin
                 (hash-table-set! var->place var var-counter)
                 (set! var-counter (+ 1 var-counter))
                 (hash-table-ref var->place var))))

           (define fact-table (make-hash-table eq?))

           (define (mk-pred-idx! pred-app)
             (let* ([vars (pred-app->vars pred-app)]
                    [name (pred->name (pred-app->pred pred-app))])
               (if (hash-table-exists? fact-table name)
                 (hash-table-set! fact-table name (cons vars (hash-table-ref fact-table name)))
                 (begin
                   (hash-table-set! fact-table name '())
                   (hash-table-set! fact-table name (cons vars (hash-table-ref fact-table name)))))))

           (define (add-fact pred-app)
               (mk-pred-idx! pred-app))

           (let* ([pred-apps (cdr (abstraction->pattern pred))])
             (begin
               (map add-fact pred-apps)
               (map (lambda (xy) (list (name->pred (first xy)) (cdr xy))) (hash-table->alist fact-table)))))

         (define (facts-to-predicate-with-vars vars facts)
           ; We want to take each place (1 2 3 ..) to a new symbol
           ; A hash table keeps track of them.

           ;; (define place-to-vars (make-hash-table eqv?))
           ;; (define (to-var place)
           ;;   (if (hash-table-exists? place-to-vars place)
           ;;     (hash-table-ref place-to-vars place)
           ;;     (begin
           ;;       (hash-table-set! place-to-vars place (sym (sample-symbol)))
           ;;       (hash-table-ref place-to-vars place))))

           ; Converts one fact to several applications
           (define (fact-to-pred-apps fact)
             (let* ([pred (first fact)]
                    [appvars (second fact)]
                    [mk-one-app (lambda (vs) `(,pred ,@vs))])
               (map mk-one-app appvars)))

           (let* ([res-sym (sym (predicate-symbol))]
                  [res-pattern 
                    `(and ,@(concatenate (map fact-to-pred-apps facts)))]
                  )
             (make-named-abstraction 
               res-sym
               res-pattern
               vars)))

         (define (facts-to-predicate facts)
           ; We want to take each place (1 2 3 ..) to a new symbol
           ; A hash table keeps track of them.

           (define place-to-vars (make-hash-table eqv?))
           (define (to-var place)
             (if (hash-table-exists? place-to-vars place)
               (hash-table-ref place-to-vars place)
               (begin
                 (hash-table-set! place-to-vars place (sym (sample-symbol)))
                 (hash-table-ref place-to-vars place))))

           ; Converts one fact to several applications
           (define (fact-to-pred-apps fact)
             (let* ([pred (first fact)]
                    [vars (second fact)]
                    [appvars (map (curry map to-var) vars)]
                    [mk-one-app (lambda (vs) `(,pred ,@vs))])
               (map mk-one-app appvars)))

           (let* ([res-sym (sym (predicate-symbol))]
                  [res-pattern 
                    `(and ,@(concatenate (map fact-to-pred-apps facts)))]
                  [res-vars (hash-table-values place-to-vars)])
             (make-named-abstraction 
               res-sym
               res-pattern
               res-vars)))

         ; represents a sample from a distribution
         (define (sample-symbol) 'x)
         ; represents a conditioning statement
         (define (predicate-symbol) 'P)


         ; Learning function applications by deriving equations
         (define (class-reps->substitutions class-reps)
           (concatenate (map (lambda (cr)
                               (let* ([c (first cr)] [r (second cr)])
                                 (map (lambda (p) (list p r)) c))) class-reps)))

         (define subs->classes connected-components-verts)

         (define (generate-substitutions pred)
           (define (revise-substitutions subs)
             (let* ([classes (subs->classes subs)]
                    [reps (find-best-representatives classes)])
               (class-reps->substitutions (zip classes reps))))
           (define (substitutions-from-eq-classes classes)
             (define (eq-class-to-equations class)
               (cond [(eq? 1 (length class)) '()]
                     [else (map (lambda (i) (list (car class) i)) (cdr class))]))
             (concatenate (map eq-class-to-equations classes)))

           (let* ([facts (pred->facts pred)]
                  [eq-classes (derive-equalities facts)]
                  )
             (revise-substitutions (append (identity-substitutions facts)
                                           (substitutions-from-eq-classes eq-classes) 
                                           (derive-addition-equations facts)
                                           (derive-neg-equations facts)))))

         ; 0. Trivial substitutions
         (define (identity-substitutions facts)
           (let* ([idx-view (view-by-indexing facts)]
                  [vars (delete-duplicates
                          (concatenate
                            (map first idx-view)))]
                  [db (begin
                        (print "(identity-substitutions) vars: ~s" vars))])
             (zip vars vars)))

         ; 1. Derives equalities from a set of facts.
         (define (get-equality-facts facts)
           (define (is-eq fact)
             (equal? equal? (first fact)))
           (filter is-eq facts))

         (define (derive-equalities facts)
           (let* ([eq-facts (get-equality-facts facts)] ; 1. filter out all eq facts
                  [eq-variable-groups (find-eq-classes eq-facts)]) ; calculate equivalence classes of variables
             eq-variable-groups)) ; return the resulting equivalence classes


         (define (fact-pred-eq p fact)
           (p (fact->pred fact)))

         (define fact->indexings second)
         (define fact->pred first)

         (define (make-fact pred indexings) (list pred indexings))

         ; Function to revise a set of facts given a set of equivalence classes of variables.
         ; 0th step, remove all equality facts.
         ; First, calculate a set of representatives for each equivalence class.
         ; Each indexing is changed so that its indices are replaced by its representatives.
         ; We then delete duplicates for each indexing.
         (define (simplify-facts eq-classes facts)
           (define var->rep (make-hash-table equal?))
           (define (calc-rep var)
             (first (first (filter (curry contains? var) eq-classes))))
           (define (to-rep var)
             (if (hash-table-exists? var->rep var)
               (hash-table-ref var->rep var)
               (begin
                 (hash-table-set! var->rep var (calc-rep var))
                 (hash-table-ref var->rep var))))
           (define (simplify-one-fact fact)
             (define (replace-with-reps indexing)
               (map to-rep indexing))
             (let* ([pred (fact->pred fact)]
                    [indexings (fact->indexings fact)])
               (make-fact pred (delete-duplicates (map replace-with-reps indexings)))))
           (let* ([no-eq (list-subtract facts (get-equality-facts facts))]
                  [simplified-facts (map simplify-one-fact no-eq)])
             simplified-facts))

         (define (flatten-facts facts)
           (concatenate (map fact->indexings facts)))

         (define (find-eq-classes eq-facts)
           (let* ([all-edges (concatenate (map fact->indexings eq-facts))]
                  [class-edges (connected-components all-edges)]
                  [var-groups (map (lambda (x) (delete-duplicates (concatenate x))) 
                                   class-edges)])
             var-groups))


         ;; Dumb algorithm to find the best representatives of equivalence classes
         (define (find-best-representatives eq-classes)
           (define (link-score pat1 pat2)
             (let* ([vs1 (pat->syms pat1)]
                    [vs2 (pat->syms pat2)]
                    [common (lset-intersection eq? vs1 vs2)]
                    )
               (/ (length common) (+ (length vs1) (length vs2)))))
           (define (covering-score reps)
             (let* ([var-usage-score (length (delete-duplicates (concatenate (map pat->syms reps))))])
               (/ (apply + (map link-score (init reps) (cdr reps))) var-usage-score)))
           (let* ([all-possible-rep-sets (apply cartesian-product eq-classes)]
                  [result (argmax covering-score all-possible-rep-sets)]
                  )
             (begin
               (print "eq-classes: ~s" eq-classes)
               (print "winning representative: ~s" result)
               result)))

                  

         (define (mk-equations-from-class class) '())

         (define (view-by-indexing facts)
           (define (fact->index-preds fact)
             (let* ([pred (fact->pred fact)]
                    [idxs (fact->indexings fact)]
                    [idx->idx-pred
                      (lambda (idx)
                        (list idx pred))])
               (map idx->idx-pred idxs)))
           (let* ([all-idx-pred (concatenate (map fact->index-preds facts))]
                  [grouped-by-idx (group-by equal? first all-idx-pred)]
                  [group->idx-view (lambda (group)
                                     (let* ([idx (first (first group))]
                                            [preds (map second group)])
                                       (list idx preds)))])
             (map group->idx-view grouped-by-idx)))

         ; Functions to create 


         (define (derive-neg-equations facts)
           (let* ([all-neg-pred (filter (curry fact-pred-eq (curry equal? neg?)) facts)]
                  [idx-view (view-by-indexing all-neg-pred)])
             (make-neg-equations idx-view)))

         (define (make-neg-equations idx-preds)
           (define (can-derive? idx-ps)
             (let* ([preds (second idx-ps)])
               (contains? neg? preds)))

           (define (get-op ps)
             (first (filter (compose not null?)
                            (map (lambda (p) (cond [(equal? neg? p) '-]
                                                   [else '()])) ps))))
           (define (make-neg-relation idx-ps)
             (let* ([idx (first idx-ps)]
                    [preds (second idx-ps)]
                    [op (get-op preds)]
                    [eq `(,(first idx) (,op ,(second idx)))])
               eq))

           (let* ([candidates (filter can-derive? idx-preds)]
                  [relations (map make-neg-relation candidates)])
             relations))

         ; 2. Derives addition equations from a set of facts (predicates + indexings)
         ; the format of an equation: idx1 === idx2 + 1 etc (some quoted list)
         ; (list lhs rhs)

         (define (derive-addition-equations facts)
           (let* ([all->-pred (filter (curry fact-pred-eq (curry equal? >)) facts)]
                  [all-diff1-pred (filter (curry fact-pred-eq (curry equal? offby1?)) facts)]
                  [all-diff2-pred (filter (curry fact-pred-eq (curry equal? offby2?)) facts)]
                  [all-diff3-pred (filter (curry fact-pred-eq (curry equal? offby3?)) facts)]
                  [idx-view (view-by-indexing
                              (append all->-pred
                                      all-diff1-pred
                                      all-diff2-pred
                                      all-diff3-pred))]
                  [equations (make-equations idx-view)])
             equations))
         (define (make-equations idx-preds) ; Possible to get a CAS to do some of this stuff?

           (define (can-derive? idx-ps)
             (let* ([preds (second idx-ps)])
               (and (or (contains? offby1? preds)
                        (contains? offby2? preds)
                        (contains? offby3? preds))
                    (or (contains? > preds)
                        (contains? < preds)))))

           (define (get-op ps)
             (first (filter (compose not null?) 
                            (map (lambda (p) (cond [(equal? > p) '+]
                                                   [(equal? < p) '-]
                                                   [else '()])) ps))))

           (define (get-const ps)
             (first (filter (compose not null?)
                            (map (lambda (p) (cond [(equal? offby1? p) 1]
                                                   [(equal? offby2? p) 2]
                                                   [(equal? offby3? p) 3])) ps))))

           (define (make-arith-relation idx-ps)
             (let* ([idx (first idx-ps)]
                    [preds (second idx-ps)]
                    [op (get-op preds)]
                    [const (get-const preds)]
                    [eq `(,(first idx) (,op ,(second idx) ,const))])

             eq))
           
           (let* ([candidates (filter can-derive? idx-preds)]
                  [relations (map make-arith-relation candidates)])
             relations))

           ; some more general algorithm:
           ; assume equivalence classes are given already
           ; go over all indexings (applications) and see where they fall on:
           ; >, <, offby1, offby2

           ; Filter them for consistency (i.e., for a particular indexing (i
           ; j), we shouldn't have (> i j) AND (< i j), or both (offby1? i j)
           ; and (offby2? i j)
           
           ; (This is looking more and more like some really basic refactoring
           ; or compiler optimization. In terms of optimizing programs, extra
           ; sharing allows one to use the same memory location for multiple
           ; variables.)
           
           ; some ad-hoc algorithm: 
           
           ; find out equivalence classses of variables use that to reduce the
           ; indexings of other predicates, so we have fewer choices find the
           ; indices that are in the intersection of indexings of > and offby1?
           ; return these indices as a set of trees, where each (directed) edge
           ; holds the relevant equation that makes it work
           
           ; later: for each of these indexings (i j) derive the equation j ===
           ; i + 1 replace all occurences of lhs with rhs (but watch out for
           ; cyclicity, in which case we might do i === j - 1) 
           ;
           ; this points to a more robust method that uses >, < and offbyn?
           ; where we obtain a spanning tree of the indexings and generate the
           ; 'proper' addition and subtraction operations <-- seems to be more
           ; general
           
           ; high level:
           ; it should be possible to generalize the transformation of
           ; predicate sets to function applications.  (see From program
           ; verification to program synthesis)
           ;
           ; how? 
           ; a set of hard-coded transformation equations?
           ; generating-and-testing function applications that match the predicates?
           

           ;(let* ([indexing-preds (facts->indexings facts)]
            ;      [consistent-indexing-preds (filter arith-consistent? indexing-preds)]
             ;     [var-tree (spanning-trees consistent-indexings)])
             ;'()))

         )
