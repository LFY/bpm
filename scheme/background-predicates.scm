(library (background-predicates)
         (export simple-hard-predicates
                 simple-range-predicates
                 simple-soft-predicates

                 unify
                 one-step-unify


                 ; Hard predicates
                 offby1?
                 offby2?
                 offby3?
                 neg?

                 ; Soft predicates
                 soft-eq?
                 soft-greater?
                 soft-offby1?
                 soft-neg?

                 ; Range predicates
                 range-eq?
                 range-greater?
                 range-offby1?
                 range-neg?

                 ; Properties of predicates
                 commutative?
                 can-apply?
                 pred->arity

                 pred->name
                 name->pred

                 pred->param

                 ; Proof rules
                 proof-rules
                 identity-rule
                 arithmetic-rule
                 eq-rule
                 neg-rule

                 ; debug
                 sig-gt
                 )
         (import (rnrs)
                 (util)
                 (_srfi :1)

                 )

         ; Hard predicates
         (define (offby1? x y)
           (equal? 1 (abs (- x y))))

         (define (offby2? x y)
           (equal? 2 (abs (- x y))))

         (define (offby3? x y)
           (equal? 3 (abs (- x y))))

         (define (neg? x y) ; This doesn't seem to work
           (equal? x (- y)))

         (define simple-hard-predicates
           (list
             equal?
             >
             offby1?
             offby2?
             offby3?
             neg?
             even?
             ))

         ; TODO: Noisy predicates

         ; Soft predicates and range predicates
         (define (gauss-diff-eq d x y)
           (/ (normal-pdf (abs (- x y)) d 0.3) (normal-pdf-max 0.3)))

         (define (gauss-eq x y)
           ((curry gauss-diff-eq 0) x y))

         (define (sig-gt x y) ; soft version of x > y
           ((shift-fx y (curry sigmoid 5.0)) x))


         (define (mk-range-pred factor thresh)
           (define (func . args)
             (let* ([fx (apply factor args)])
               (> fx thresh)))
           func)

         (define (range-eq? x y) ((mk-range-pred gauss-eq 1.0) x y))
         (define (range-greater? x y) ((mk-range-pred sig-gt 0.9) x y))
         (define (range-offby1? x y) ((mk-range-pred (curry gauss-diff-eq 1.0) 1.0) x y))
         (define (range-neg? x y) ((mk-range-pred (lambda (x y) (gauss-eq (- x) y)) 1.0) x y))

         (define simple-range-predicates
           (list
             range-eq?
             range-greater?
             range-offby1?
             range-neg?))

         (define soft-eq? gauss-eq)
         (define soft-greater? sig-gt)
         (define soft-offby1? (curry gauss-diff-eq 1.0))
         (define (soft-neg? x y) (gauss-eq (- x) y))

         (define simple-soft-predicates
           (list
             soft-eq?
             soft-greater?
             soft-offby1?
             soft-neg?))

         ; Typechecker
         (define (can-apply? p args)
           ; account for holes. currently, we allow holes in the argument to typecheck always.
           ; this is accomplished by removing all holes from the argument.
           (let* ([real-args (filter (lambda (a) (not (eq? 'H a))) args)])
             (cond 
               ; Hard predicates
               [(equal? p equal?) #t]
               [(equal? p >) (are-all number? real-args)]
               [(equal? p offby1?) (are-all number? real-args)]
               [(equal? p offby2?) (are-all number? real-args)]
               [(equal? p offby3?) (are-all number? real-args)]
               [(equal? p neg?) (are-all number? real-args)]
               [(equal? p even?) (are-all integer? real-args)]


               ; Range predicates
               [(or (equal? p range-eq?)
                    (equal? p range-greater?)
                    (equal? p range-offby1?)
                    (equal? p range-neg?)) (are-all number? real-args)]

               ; Soft predicates
               [(or (equal? p soft-eq?)
                    (equal? p soft-greater?)
                    (equal? p soft-offby1?)
                    (equal? p soft-neg?)) (are-all number? real-args)]

               [else #f])))

         ; Commutativity
         (define (commutative? p)
           (cond 
             ; General
             [(equal? (pred->arity p) 1) #t]

             ; Hard predicates
             [(equal? p >) #f]
             [(equal? p equal?) #t]
             [(equal? p offby1?) #t]
             [(equal? p offby2?) #t]
             [(equal? p offby3?) #t]
             [(equal? p neg?) #t]

             ; Soft predicates
             [(equal? p range-eq?) #t]
             [(equal? p range-greater?) #f]
             [(equal? p range-offby1?) #t]
             [(equal? p range-neg?) #t]

             [(equal? p soft-eq?) #t]
             [(equal? p soft-greater?) #f]
             [(equal? p soft-offby1?) #t]
             [(equal? p soft-neg?) #t]

             [else #f]))

         ; Arity
         (define (pred->arity p)
           (cond 
             ; Hard predicates
             [(equal? equal? p) 2]
             [(equal? > p) 2]
             [(equal? offby1? p) 2]
             [(equal? offby2? p) 2]
             [(equal? offby3? p) 2]
             [(equal? neg? p) 2]
             [(equal? even? p) 1]

             ; Soft predicates
             [(equal? p range-eq?) 2]
             [(equal? p range-greater?) 2]
             [(equal? p range-offby1?) 2]
             [(equal? p range-neg?) 2]

             [(equal? p soft-eq?) 2]
             [(equal? p soft-greater?) 2]
             [(equal? p soft-offby1?) 2]
             [(equal? p soft-neg?) 2]
             ))

         ; Names
         (define (pred->name p)
           (cond
             ; Hard predicates
             [(equal? equal? p) "equal?"]
             [(equal? > p) ">"]
             [(equal? offby1? p) "offby1?"]
             [(equal? offby2? p) "offby2?"]
             [(equal? offby3? p) "offby3?"]
             [(equal? neg? p) "neg?"]
             [(equal? even? p) "even?"]

             ; Soft predicates
             [(equal? p range-eq?) "range-eq?"]
             [(equal? p range-greater?) "range-greater?"]
             [(equal? p range-offby1?) "range-offby1?"]
             [(equal? p range-neg?) "range-neg?"]

             [(equal? p soft-eq?) "soft-eq?"]
             [(equal? p soft-greater?) "soft-greater?"]
             [(equal? p soft-offby1?) "soft-offby1?"]
             [(equal? p soft-neg?) "soft-neg?"]
             ))

         (define (name->pred name)
           (cond
             ; Hard predicates
             [(equal? "equal?" name) equal?]
             [(equal? ">" name) >]
             [(equal? "offby1?" name) offby1?]
             [(equal? "offby2?" name) offby2?]
             [(equal? "offby3?" name) offby3?]
             [(equal? "neg?" name) neg?]
             [(equal? "even?" name) even?]

             ; Soft predicates
             [(equal? name "range-eq?") range-eq?]
             [(equal? name "range-greater?") range-greater?]
             [(equal? name "range-offby1?") range-offby1?]
             [(equal? name "range-neg?") range-neg?]

             [(equal? name "soft-eq?") soft-eq?]
             [(equal? name "soft-greater?") soft-greater?]
             [(equal? name "soft-offby1?") soft-offby1?]
             [(equal? name "soft-neg?") soft-neg?]))

         ; Small-step unifications of partially-applied predicates
         ; Returns a list of possible solutions to the holes (usu. 1 or 2, else '? is
         ; returned to represent a very large number of solutions)
         ; The input is a predicate and a list of arguments,
         ; each of which may have holes (denoted by 'H)).
         ; If there are no holes in the arguments, we return whatever the predicate evaluates to.

         ; Should be able to do this more easily in prolog...

         (define (fully-applied? args)
           (not (contains? 'H args)))

         (define (hole-at? i args)
           (eq? 'H (list-ref i args)))

         (define (hole-count args)
           (length (filter (lambda (x) (eq? 'H x)) args)))

         (define (can-unify? args)
           (eq? 1 (hole-count args)))

         (define (unify-one-pred-app p args)
           ; Count the number of holes. if 0, apply the predicate.
           ; if 1, return the unification (possible solution).
           ; else, return '?
           (let* ([num-holes (hole-count args)])
             (cond [(eq? 0 num-holes) (apply p args)]
                   [(eq? 1 num-holes) (one-step-unify p args)]
                   [else '?])))


         (define (one-step-unify p args)
           (define (eq-unification x ivs)
             (list (second (first ivs))))
           (define (offby1-unification x ivs)
             (let* ([v (second (first ivs))])
               (list (+ v 1) (- v 1))))
           (define (neg-unification x ivs)
             (list (- (second (first ivs)))))
           (let* ([hole-position (list-index (curry eq? 'H) args)]
                  [non-hole-positions (filter (lambda (ix) (not (eq? hole-position (first ix))))
                                              (zip (iota (length args)) args))]

                  [second-arg (second args)])
             (cond [(equal? p soft-eq?) (eq-unification hole-position non-hole-positions)]
                   [(equal? p soft-greater?) '()]
                   [(equal? p soft-offby1?) (offby1-unification hole-position non-hole-positions)]
                   [(equal? p soft-neg?) (neg-unification hole-position non-hole-positions)])))

         ; unifies multiple predicate applications where 'H occurs once in each predicate application.
         ; returns a list of possible solutions (possibly '())
         ; when we use this, we'd like to only accept refinements that have one solution
         (define (unify pred-apps)
           (let* ([pred-app-sols (map (lambda (pas) (unify-one-pred-app (first pas) (second pas))) pred-apps)])
             (apply (curry lset-intersection eq?) pred-app-sols)))

         (define (pred->param p)
           (cond
             [(equal? offby1? p) 1]
             [(equal? offby1? p) 2]
             [(equal? offby1? p) 3]
             [else '()]))

         ; proof rules
         ; each proof rule takes idx-predicates to a list of equations, possibly empty
         (define proof-rules
           (list identity-rule
                 arithmetic-rule
                 neg-rule
                 eq-rule))

         (define (is-gt? p) (or (eq? > p)
                                (eq? soft-greater? p)))

         (define (identity-rule idx-ps)
           (let* ([idx (first idx-ps)])
             (zip idx idx)))

         (define (arithmetic-rule idx-ps)
           (let* ([ps (second idx-ps)]
                  [idx (first idx-ps)]
                  [op (cond [(or (contains? > ps)
                                 (contains? soft-greater? ps)) '+]
                            [(contains? < ps) '-]
                            [else '()])]
                  [const (cond [(or (contains? offby1? ps)
                                    (contains? soft-offby1? ps)) 1]
                               [(contains? offby2? ps) 2]
                               [(contains? offby3? ps) 3]
                               [else '()])])
             (if (or (null? op) (null? const)) '()
               (list `(,(first idx) (,op ,(second idx) ,const))))))

         (define (neg-rule idx-ps)
           (let* ([ps (second idx-ps)]
                  [idx (first idx-ps)]
                  [op (cond [(contains? neg? ps) '-]
                            [else '()])]
                  )
             (if (null? op) '()
               (list `(,(first idx) (,op ,(second idx)))))
             ))

         (define (eq-rule idx-ps)
           (let* ([ps (second idx-ps)]
                  [idx (first idx-ps)]
                  [op (cond [(or (contains? soft-eq? ps)
                                 (contains? equal? ps)) 'EQ]
                            [else '()])]
                  )
             (if (null? op) '()
               (list `(,(first idx) ,(second idx))))
             ))
         )
