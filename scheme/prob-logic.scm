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
                 learn-facts
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
                      [idx (second pred-idx)]
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

         (define (learn-predicates prog abstr)
           (let* ([mat (arg-matrix prog abstr)] ; column-major
                  [rows (apply zip mat)] ; row-major
                  [row-preds (map get-all-facts rows)]
                  [remaining (find-common-facts row-preds)])
                  ;; [db (begin
                  ;;       (print "resulting facts: ~s" remaining))])
             (facts-to-predicate remaining)))

         (define (learn-predicates-keep-vars prog abstr)
           (let* ([vars (abstraction->vars abstr)]
                  [mat (arg-matrix prog abstr)]
                  [rows (apply zip mat)]
                  [row-preds (map (curry get-all-facts-with-vars vars) rows)]
                  [remaining (find-common-facts row-preds)])
                  ;; [db (begin
                  ;;       (print "resulting facts: ~s" remaining))])
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

         ; accessors

         (define fact->indexings second)
         (define fact->pred first)

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
             (begin (print "classes: ~s" eq-classes) (print "best representatives: ~s" result)
             result)
             ))

         ;; Smarter algorithm to find the best representatives

         (define (find-best-reps-interleave-substitution eq-classes)
           (define (mk-rep pat) (list 'rep pat))
           (define (mk-class pats) (list 'class pats))

           (define (class? x) (eq? 'class (car x)))
           (define (rep? x) (eq? 'rep (car x)))

           (define (var-in? var pat)
             (contains? var (pat->syms pat)))

           (define class->pats second)
           (define rep->pat second)

           (define starting-classes (map mk-class eq-classes))
           (define starting-reps (map (lambda (x) (mk-rep '())) eq-classes))

           (define (rep-exists? class-rep) (not (null? (rep->pat (second class-rep)))))

           (define (first-result-or-null xs)
             (if (null? xs) '()
               (first xs)))

           (define (class->var->rep var class)
             (let* ([pats (class->pats class)])
               (mk-rep (first-result-or-null (filter (curry var-in? var) pats)))))

           (define (most-common-variable eq-classes current-substitutions)
             (define (var-count var patterns)
               (+ (length (filter (lambda (p) (contains? var (pat->syms p))) (map second current-substitutions)))
                  (length (filter (lambda (p) (contains? var (pat->syms p))) patterns))))
             (let* ([all-pats (concatenate (map class->pats eq-classes))]
                    [all-vars (concatenate (map pat->syms all-pats))]
                    )
               (argmax (lambda (v) (var-count v all-pats)) all-vars)))

           (define (apply-rep subs work-list)
             (define (transform-pattern pat)
               (let* ([vars (pat->syms pat)] ;; just find the first substitution that can replace that var
                      [var->sub 
                        (lambda (v) 
                          (let* (
                                 [results 
                                   (filter (lambda (p) (not (eq? v p))) 
                                           (concatenate (filter (lambda (lr) 
                                                                  (contains? v (concatenate (map pat->syms lr)))) subs)))])
                            (if (null? results) v
                              (car results))))]
                      [replace-next (lambda (v pat)
                                      (sexp-replace v (var->sub v) pat))])
                 (fold replace-next pat vars)
                 ))
             (map (lambda (class) (mk-class (map transform-pattern (class->pats class)))) work-list))

           (define (loop acc classes)
             (if (null? classes) acc
               (let* (
                      [mcv (most-common-variable classes acc)]
                      [new-reps (map (curry class->var->rep mcv) classes)] 
                      [new-class-reps (zip classes new-reps)]
                      [current-results (class-reps->substitutions (map (lambda (cr) (list (class->pats (first cr))
                                                                                          (rep->pat (second cr)))) (filter (lambda (x) (rep-exists? x)) new-class-reps)))]
                      [work-list (map first (filter (lambda (x) (not (rep-exists? x))) new-class-reps))]
                      [next-classes (apply-rep current-results work-list)] ;; interleaving substitution
                      )
                 (loop (append current-results acc) next-classes))))
           (loop '() starting-classes))

         
         ; Generating substitutions
         (define (generate-substitutions pred)
           (define (revise-substitutions subs)
             (let* ([classes (subs->classes subs)]
                    [result (find-best-reps-interleave-substitution classes)]
                    )
            result
             ))
           (let* ([facts (pred->facts pred)]
                  [result (revise-substitutions (derive-general proof-rules (pred->facts pred)))]
                  )
                    result))

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

         (define (derive-general proof-rules facts)
           (define (mk-equations idx-ps)
             (concatenate (map (lambda (f) (f idx-ps)) proof-rules)))
           (let* ([idx-view (view-by-indexing facts)])
             (concatenate (map mk-equations idx-view))))

         )
