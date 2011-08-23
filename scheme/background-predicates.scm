(library (background-predicates)
         (export simple-hard-predicates
                 simple-range-predicates

                 simple-soft-predicates
                 simple-soft-parameterized-predicates

                 all-soft-predicates

                 parameterized-predicates

                 unify
                 one-step-unify

                 ; Hard predicates
                 offby1?
                 offby2?
                 offby3?
                 neg?
                 twice?

                 ; parameterized:
                 offbyN?
                 ratio?

                 ; parameterized predicate utility functions
                 param-pred?
                 param-pred-fx?
                 mk-param-pred
                 apply-param-pred
                 derive-param-pred
                 derive-param-pred-average


                 ; Soft predicates
                 soft-eq?
                 soft-greater?
                 soft-offby1?
                 soft-neg?

                 ; Soft parameterized predicates
                 soft-offbyN?
                 soft-ratio?

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

                 param-pred->params
                 param-pred->fx


                 ; Equality of predicates

                 ; Proof rules
                 proof-rules
                 identity-rule
                 arithmetic-rule
                 arithmetic-rule2
                 eq-rule
                 neg-rule

                 ; debug
                 sig-gt
                 )
         (import (rnrs)
                 (util)
                 (_srfi :1)
                 (printing)

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

         (define (twice? x y)
           (equal? (* 2 y) x))


         (define simple-hard-predicates
           (list
             equal?
             >
             offby1?
             offby2?
             offby3?
             neg?
             even?
             twice?
             ))

         (define (offbyN? n x y)
           (equal? n (abs (- x y))))

         (define (ratio? n x y)
           (cond [(or (= 0 x) (= 0 y)) #f]
                 [(or (equal? n (/ x y))
                      (equal? n (/ y x))) #t]
                 [else #f]))

         ; format of a parameterized predicate:
         ; (list p param1 param2 ...)
         
         (define (mk-param-pred fx . ps)
           (append (list 'PARAM-PRED fx) ps))

         (define (param-pred? p) 
           (cond [(list? p) (eq? 'PARAM-PRED (car p))]
                 [else #f]))

         (define (param-pred-fx? p)
           (contains? p (list soft-offbyN?
                              soft-ratio?
                              offbyN?
                              ratio?)))

         (define param-pred->fx second)
         (define (param-pred->params p) (cdr (cdr p)))

         (define (apply-param-pred pred+params . args)
           (let* ([params (param-pred->params pred+params)]
                  [fx (param-pred->fx pred+params)])
             (apply fx (append params args))))

         ; assumes that the number of args already agrees and that it typechecks
         ; returns null if can't derive parameter
         (define (derive-param-pred fx . args)
           (cond 
             [(or (equal? offbyN? fx)
                  (equal? soft-offbyN? fx)) (mk-param-pred fx (abs (- (first args) (second args))))]
             [(or (equal? ratio? fx)
                  (equal? soft-ratio? fx)) (cond [(or (contains? 0 args)
                                                      (contains? +nan.0 args)
                                                      (contains? -nan.0 args)) '()]
                  [else (mk-param-pred fx (/ (first args) (second args)))])]
             [else '()]))

         (define (derive-param-pred-average fx . arg-sets)
           (let* ([derived-param-preds (filter (lambda (x) (not (null? x))) (map (lambda (args) (apply (curry derive-param-pred fx) args)) arg-sets))]
                  [params (map param-pred->params derived-param-preds)]
                  [mean-param (map my-mean (if (> (length params) 1) (apply zip params) params))])
             (if (null? mean-param) '()
               (apply (curry mk-param-pred fx) mean-param))))

         (define parameterized-predicates
           (list
             offbyN?
             ratio?))

         ; TODO: Noisy predicates

         ; Soft predicates and range predicates
         (define (gauss-diff-eq d x y)
           (/ (normal-pdf (abs (- x y)) d 0.3) (normal-pdf-max 0.3)))

         (define (gauss-ratio-eq d x y)
           (if (or (= x 0) (= y 0)) 0.000000001
             (/ (normal-pdf (/ x y) d 0.3) (normal-pdf-max 0.3))))

         (define (gauss-eq x y)
           ((curry gauss-diff-eq 0) x y))

         (define (sig-gt x y) ; soft version of x > y
           ((shift-fx y (curry sigmoid 10.0)) x))


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

         (define soft-offbyN? gauss-diff-eq)
         (define soft-ratio? gauss-ratio-eq)

         (define simple-soft-parameterized-predicates
           (list
             soft-offbyN?
             soft-ratio?
             ))

         (define all-soft-predicates (append simple-soft-predicates
                                             simple-soft-parameterized-predicates))

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
               [(equal? p twice?) (are-all number? real-args)]

               ; Parameterized predicates
               [(param-pred? p) (let* ([fx (param-pred->fx p)])
                                  (cond [(equal? offbyN? fx) (are-all number? real-args)]
                                        [(equal? ratio? fx) (are-all number? real-args)]
                                        [(equal? p soft-offbyN?) (are-all number? real-args)]
                                        [(equal? p soft-ratio?) (are-all number? real-args)]
                                        [else '()]))]

               [(equal? p offbyN?) (are-all number? real-args)]
               [(equal? p ratio?) (are-all number? real-args)]

               [(equal? p soft-offbyN?) (are-all number? real-args)]
               [(equal? p soft-ratio?) (are-all number? real-args)]

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
             [(equal? p twice?) #f]

             ; Parameterized predicates
             [(param-pred? p) (let* ([fx (param-pred->fx p)])
                                (cond [(equal? offbyN? fx) #t]
                                      [(equal? ratio? fx) #t]
                                      [(equal? p soft-offbyN?) #t]
                                      [(equal? p soft-ratio?) #t]
                                      [else '()]))]
             [(equal? p offbyN?) #t]
             [(equal? p ratio?) #t]
             [(equal? p soft-offbyN?) #t]
             [(equal? p soft-ratio?) #t]

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
             [(equal? twice? p) 2]

             ; Parameterized predicates
             [(param-pred? p) (let* ([fx (param-pred->fx p)])
                                (cond 
                                  [(equal? offbyN? fx) 2]
                                  [(equal? ratio? fx) 2]
                                  [(equal? p soft-offbyN?) 2]
                                  [(equal? p soft-ratio?) 2]
                                  [else '()]))]

             ; Raw form
             [(equal? offbyN? p) 2]
             [(equal? ratio? p) 2]
             [(equal? p soft-offbyN?) 2]
             [(equal? p soft-ratio?) 2]

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
             [(equal? twice? p) "twice?"]

             ; Parameterized predicates
             ; name format: actually a list (list name param)
             [(param-pred? p) (let* ([fx (param-pred->fx p)]
                                     [p (param-pred->params p)])
                                (cond 
                                  [(equal? offbyN? fx) (cons "offbyN?" p)]
                                  [(equal? ratio? fx) (cons "ratio?" p)]
                                  [(equal? soft-offbyN? fx) (cons "soft-offbyN?" p)]
                                  [(equal? soft-ratio? fx) (cons "soft-ratio?" p)]
                                  [else '()]))]

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
             [(equal? "twice?" name) twice?]

             ; Parameterized predicates
             [(list? name) (let* ([str (car name)]
                                  [params (cdr name)])
                             (cond 
                               [(equal? "offbyN?" str) (apply (curry mk-param-pred offbyN?) params)]
                               [(equal? "ratio?" str) (apply (curry mk-param-pred ratio?) params)]
                               [(equal? "soft-offbyN?" str) (apply (curry mk-param-pred soft-offbyN?) params)]
                               [(equal? "soft-ratio?" str) (apply (curry mk-param-pred soft-ratio?) params)]
                               [else '()]))]

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
             [(equal? offby2? p) 2]
             [(equal? offby3? p) 3]
             [(param-pred? p) (param-pred->params p)]
             [else '()]))

         ; proof rules
         ; each proof rule takes idx-predicates to a list of equations, possibly empty
         (define proof-rules
           (list identity-rule
                 arithmetic-rule
                 arithmetic-rule2
                 arithmetic-rule3
                 neg-rule
                 eq-rule
                 twice-rule))

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

         (define (arithmetic-rule2 idx-ps)
           (let* ([ps (second idx-ps)]
                  [idx (first idx-ps)]
                  [op (cond [(or (contains? > ps)
                                 (contains? soft-greater? ps)) '+]
                            [(contains? < ps) '-]
                            [else '()])]
                  [const (let* ([param-preds (filter param-pred? ps)]
                                [fxs (map param-pred->fx param-preds)]
                                [params (map param-pred->params param-preds)]
                                [fx-params (zip fxs params)]
                                [candidate-params 
                                  (filter (lambda (x) (not (null? x))) 
                                          (map (lambda (f-ps)
                                                 (let* ([fx (first f-ps)])
                                                   (cond [(or (eq? fx offbyN?)
                                                              (eq? fx soft-offbyN?)
                                                              (eq? fx gauss-diff-eq)) (car (second f-ps))]
                                                         [else '()]))) fx-params))])
                           (if (null? candidate-params) '()
                             (car candidate-params)))])
             (if (or (null? op) (null? const)) '()
               (list `(,(first idx) (,op ,(second idx) ,const))))))

         (define (arithmetic-rule3 idx-ps)
           (let* ([ps (second idx-ps)]
                  [idx (first idx-ps)]
                  [op (cond [(or (contains? > ps)
                                 (contains? soft-greater? ps)) '*]
                            [(contains? < ps) '/]
                            [else '()])]
                  [const (let* ([param-preds (filter param-pred? ps)]
                                [fxs (map param-pred->fx param-preds)]
                                [params (map param-pred->params param-preds)]
                                [fx-params (zip fxs params)]
                                [candidate-params 
                                  (filter (lambda (x) (not (null? x))) 
                                          (map (lambda (f-ps)
                                                 (let* ([fx (first f-ps)])
                                                   (cond [(or (eq? fx ratio?)
                                                              (eq? fx soft-ratio?)
                                                              (eq? fx gauss-ratio-eq)) (car (second f-ps))]
                                                         [else '()]))) fx-params))])
                           (if (null? candidate-params) '()
                             (car candidate-params)))])
             (if (or (null? op) (null? const)) '()
               (list `(,(first idx) (,op ,(second idx) ,const))))))

         (define (neg-rule idx-ps)
           (let* ([ps (second idx-ps)]
                  [idx (first idx-ps)]
                  [op (cond [(or (contains? neg? ps)
                                 (contains? soft-neg? ps)) '-]
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

         (define (twice-rule idx-ps)
           (let* ([ps (second idx-ps)]
                  [idx (first idx-ps)]
                  [op (cond [(contains? twice? ps) 'MUL]
                            [else '()])]
                  )
             (if (null? op) '()
               (list `(,(first idx) (* 2 ,(second idx)))))
             ))
         )

