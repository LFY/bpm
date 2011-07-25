(library (background-predicates)
         (export simple-hard-predicates
                 simple-range-predicates
                 simple-soft-predicates
                 
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
                 pred->param

                 ; debug
                 sig-gt
                 )
         (import (rnrs)
                 (util))

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
           (cond 
             ; Hard predicates
             [(equal? p equal?) #t]
             [(equal? p >) (are-all number? args)]
             [(equal? p offby1?) (are-all number? args)]
             [(equal? p offby2?) (are-all number? args)]
             [(equal? p offby3?) (are-all number? args)]
             [(equal? p neg?) (are-all number? args)]
             [(equal? p even?) (are-all integer? args)]


             ; Range predicates
             [(or (equal? p range-eq?)
                  (equal? p range-greater?)
                  (equal? p range-offby1?)
                  (equal? p range-neg?)) (are-all number? args)]

             ; Soft predicates
             [(or (equal? p soft-eq?)
                  (equal? p soft-greater?)
                  (equal? p soft-offby1?)
                  (equal? p soft-neg?)) (are-all number? args)]

             [else #f]))

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

         (define (pred->param p)
           (cond
             [(equal? offby1? p) 1]
             [(equal? offby1? p) 2]
             [(equal? offby1? p) 3]
             [else '()]))

         )
