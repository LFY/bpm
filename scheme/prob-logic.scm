(library (prob-logic)
         (export learn-predicates
                 learn-soft-predicates
                 feature-induction
                 feature-induction-n-iter

                 sample-symbol
                 predicate-symbol
                 print-hypothesis-score

                 ; debug
                 get-all-soft-facts
                 find-common-soft-facts
                 facts-to-predicate
                 soft-facts-to-predicate
                 derive-equalities
                 derive-addition-equations
                 learn-facts
                 simplify-facts

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

         ; this is a breadth-first search (for now) (possible modifications: bounded-depth search, iterative deepening)

         ; what happens if there are no refinements...

         (define (induce-one-step background data current-hypothesis)
           (let* ([refinements (get-refinements background data current-hypothesis)]
                  [refinement-scores (zip refinements
                                          (map (curry data-hyp->log-likelihood data) refinements))]
                  [best-refinement-score (argmax second refinement-scores)])
             best-refinement-score))

         (define (feature-induction threshold background data current-hypothesis)
           (let* ([new-hyp-score (induce-one-step background data current-hypothesis)])
             (cond [(null? new-hyp-score) (begin (print "Could not find valid refinement of current hypothesis. Stopping.")
                                                 current-hypothesis)] ; couldn't find any legal refinements.
                   [(> threshold (second new-hyp-score)) (begin (print "No refinement is above threshold. Stopping.")
                                                                current-hypothesis)] ; score of the new hypothesis is less than threshold.
                   [else (begin (print "Continuing with new hypothesis:")
                                (print-hypothesis-score new-hyp-score)
                                (feature-induction threshold background data (first new-hyp-score)))]))) ; keep going otherwise

         (define (feature-induction-n-iter n background data current-hypothesis)
           (let* ([new-hyp-score (induce-one-step background data current-hypothesis)])
             (cond [(null? new-hyp-score) (begin (print "Could not find valid refinement (~d iterations left). Stopping with current hypothesis." (+ n 1))

                                                 current-hypothesis)]
                   [(eq? 0 n) (begin (print "No more iterations. The resulting hypothesis:")
                                     (print-hypothesis-score new-hyp-score))]
                   [else (begin (print "Iterations left: ~d:" (+ n 1))
                                (print-hypothesis-score new-hyp-score)
                                (feature-induction-n-iter (- n 1) background data (first new-hyp-score)))])))



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

           (define (already-used? pred-idx)
             (contains? pred-idx current-hypothesis))

           (define (legal-app? pred-idx)
             (and (not (already-used? pred-idx))
                  (can-apply? (first pred-idx) 
                              (idx->vals (first data) (second pred-idx)))))

           (let* ([applications (generate-predicate-applications)]
                  [legal-refinements (filter legal-app? applications)])
             (map (lambda (r) (cons r current-hypothesis)) legal-refinements)))

         (define (idx->vals row idx)
           ;(display-all "idx->vals: first arg: " row " second arg: " idx "\n")
           (map (lambda (i) (list-ref row i)) idx))

         (define (data-hyp->log-likelihood data hyp)
           (define (single-log-likelihood row)
             (define (apply-one-pred pred-idx)
               (let* ([pred (first pred-idx)]
                      [idx (second pred-idx)]
                      [val (log (apply pred (idx->vals row idx)))])
                 val))
                 (apply + (map apply-one-pred hyp)))
           (apply + (map single-log-likelihood data)))

         (define (argmax f xs)
           (cond [(null? xs) '()]
                 [else 
                   (first (sort (lambda (x y)
                                  (> (f x) (f y))) xs))]))

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
                  [remaining (find-common-facts row-preds)])
             (facts-to-predicate remaining)))

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


         (define (find-common-facts fact-sets)
           (define (in-all-sets fact)
             (conj (map (lambda (fset) (contains? fact fset)) fact-sets)))
           (let* ([all-facts (delete-duplicates (concatenate fact-sets))]
                  [remaining (filter in-all-sets all-facts)])
             remaining))

         ; Converts the predicate to an abstraction

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

         (define (find-eq-classes eq-facts)
           (let* ([all-edges (concatenate (map fact->indexings eq-facts))]
                  [class-edges (connected-components all-edges)]
                  [var-groups (map (fcomp delete-duplicates concatenate) class-edges)])
             var-groups))

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
         ; 2. Derives addition equations from a set of facts (predicates + indexings)
         ; the format of an equation: idx1 === idx2 + 1 etc (some quoted list)
         ; (list lhs rhs)

         (define (derive-addition-equations facts)
           (let* ([all-<-pred (filter (curry fact-pred-eq (curry equal? >)) facts)]
                  [all-diff1-pred (filter (curry fact-pred-eq (curry equal? offby1?)) facts)]
                  [all-diff2-pred (filter (curry fact-pred-eq (curry equal? offby2?)) facts)]
                  [all-diff3-pred (filter (curry fact-pred-eq (curry equal? offby3?)) facts)]
                  [idx-view (view-by-indexing
                              (append all-<-pred
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
                            (map (lambda (p) (cond [(equal? > p) '-]
                                                   [(equal? < p) '+]
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
                    [eq `(x (,op ,const y))])

             (list idx eq)))
           
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
