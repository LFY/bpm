;;TO DO
;;-adjust tree-apply-proc to not be dependent on * as a masking character
;;-use data abstraction for location in tree-apply-proc
(library (util)
         (export all-equal? all-assoc curry all max-take sexp-replace sexp-search get/make-alist-entry rest pair depth tree-apply-proc primitive? non-empty-list? all-subexprs deep-find-all map-apply more-than-one primitives list-unique-commutative-pairs unique-commutative-pairs my-mean my-variance thunkify normal-pdf deep-find display-all tagged-list? list-or are-all


                 println

                 shallow-find-all
                 sexp-replace-pred

                 contains?
                 group-by
                 sort
                 sort-by
                 median-split
                 list-subtract
                 init
                 chop-last
                 iterate
                 ngram
                 list-remove-at
                 list-remove-at-several
                 list-idxs-where
                 split-into

                 conj
                 disj

                 normal-pdf-max
                 sigmoid
                 shift-fx
                 reflect-fx

                 fcomp
                 compose

                 ;; sampling functions
                 random-from-range
                 sample-gaussian
                 uniform-sample

                 rnd-select
                 uniform-select
                 rnd-drop-list

                 sexp-walk
                 subexpr-walk
                 replace-car

                 ;; process stuff
                 string->sexpr

                 with-input-from-string
                 process
                 bytevector->string
                 get-bytevector-all
                 native-transcoder

                 ;; sorting s-expressions
                 sexpr-sort

                 ;; log probabilities
                 log-prob-sum
                 log-prob-sum2

                 define-opt

                 str-split
                 )
         (import (except (rnrs) string-hash string-ci-hash)
                 (opt-args)
                 (printing)
                 (_srfi :1)
                 (_srfi :69)
                 (srfi :67)
                 (only (ikarus) 
                       with-input-from-string 
                       process
                       bytevector->string
                       get-bytevector-all
                       native-transcoder)
                 (church readable-scheme)
                 )


         (define (thunkify sexpr) `(lambda () ,sexpr))
         (define (all-equal? lst)
           (all (map (lambda (itm) (equal? (first lst) itm)) (rest lst))))

         (define (all-assoc alist key)
           (filter (lambda (entry) (equal? (first entry) key)) alist))

         (define (curry fun . const-args)
           (lambda args
             (apply fun (append const-args args))))

         (define (fcomp . fs)
           (define (comp f g)
             (lambda (x) (f (g x))))
           (fold comp (lambda (x) x) fs))


         ;(define (compose . fs)
         ;  (define (loop res rem-fs)
         ;    (cond [(null? fs) res]
         ;          [else (loop 
         ;                  (lambda args (apply (first rem-fs) (apply res args))) 
         ;                  (rest rem-fs))]))
         ;  (loop (lambda (x) x) fs))

         (define (all lst)
           (if (null? lst)
             #t
             (and (first lst)
                  (all (rest lst)))))

         (define (are-all pred lst)
           (all (map pred lst)))

         (define (max-take lst n)
           (if (<= (length lst) n)
             lst
             (take lst n)))
         ;;if abstract.ss stops working replace sexp-replace in abstract.ss w/ a function leaf replace (sexp-replace before it was modified to the current version)
         (define (sexp-replace old new sexp)
           (if (equal? old sexp)
             new
             (if (list? sexp)
               (map (curry sexp-replace old new) sexp)
               sexp)))

         (define (sexp-replace-pred pred? new sexp)
           (if (pred? sexp)
             new
             (if (list? sexp)
               (map (curry sexp-replace-pred pred? new) sexp)
               sexp)))


         (define (sexp-search pred? func sexp)
           (if (pred? sexp)
             (func sexp)
             (if (list? sexp)
               (map (curry sexp-search pred? func) sexp)
               sexp)))

         ;;should stop early due to the way or works, not clear what first found instance will be
         (define (deep-find pred? sexp)
           (if (pred? sexp)
             sexp
             (if (list? sexp)
               (list-or (map (curry deep-find pred?) sexp))
               (pred? sexp))))

         ;from stack-overflow, takes place of (apply or some-list)
         (define (list-or args)
           (if (null? args)
             #f
             (or (first args) (list-or (rest args)))))



         (define (deep-find-all pred? sexp)
           (filter pred? (all-subexprs sexp)))

         (define (primitives expr)
           (if (primitive? expr)
             (list expr)
             (apply append (map primitives expr))))
         ;;does not return primitives, only subexpressions which are lists
         (define (all-subexprs t)
           (let loop ([t (list t)])
             (cond [(null? t) '()]
                   [(primitive? (first t)) (loop (rest t))]
                   [else (pair (first t) (loop (append (first t) (rest t))))])))

         (define (shallow-find-all pred? sexp)
           (define (loop t)
             (cond [(null? t) '()]
                   [(pred? (car t)) (list t)]
                   [(primitive? (car t)) (loop (cdr t))]
                   [else 
                     (append (loop (car t)) 
                             (loop (cdr t)))]))
           (loop sexp))


         ;; look up value for key in alist; if not found,
         ;; set (default-thunk) as value and return it
         (define (get/make-alist-entry alist alist-set! key default-thunk)
           (let ([binding (assq key alist)])
             (if binding
               (rest binding)
               (let* ([default-val (default-thunk)])
                 (alist-set! key default-val)
                 default-val))))

         (define (random-from-range a b)
           (+ (random-integer (+ (- b a) 1)) a))

         (define (uniform-sample a b)
           (+ a (* (- b a) (random-real))))

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

         (define (uniform-select vs)
           (let* ([p (/ 1.0 (length vs))])
             (cadr (rnd-select (map (lambda (v) (list p v)) vs)))))

         (define (rnd-drop-list prob xs)
           (define (loop acc xs)
             (cond [(null? xs) acc]
                   [(< prob (uniform-sample 0 1)) (loop acc (cdr xs))]
                   [else (loop (cons (car xs) acc) (cdr xs))]))
           (loop '() xs))

         ;; end sampling functions

         (define (primitive? expr)
           (or (symbol? expr) (boolean? expr) (number? expr) (quoted? expr)))

         (define (non-empty-list? expr)
           (if (list? expr)
             (if (null? expr)
               #f
               #t)
             #f))

         (define (more-than-one lst)
           (> (length lst) 1))

         (define (quoted? expr)
           (if (non-empty-list? expr)
             (eq? (first expr) 'quote)))


         (define (string->sexpr str)
           (with-input-from-string str read))

         (define (depth tree)
           (if (or (not (list? tree)) (null? tree))
             0
             (+ 1 (apply max (map depth tree)))))

         ;;creates a lambda that performs an apply for the passed in function
         ;;useful for map over arguments produced by some other function
         (define (lambda-apply proc)
           (lambda (arg-list) (apply proc arg-list)))

         (define (map-apply proc arg-lists)
           (map (lambda-apply proc) arg-lists))

         ;;this function copies a tree, but applies proc to the tree at the passed in location.  
         (define (tree-apply-proc proc location tree)
           (define (build-mask location number-of-branches)
             (define (substitute indx value lst)
               (if (= indx 0)
                 (pair value (rest lst))
                 (pair (first lst) (substitute (- indx 1) value (rest lst)))))
             (let ([mask (make-list number-of-branches '*)]
                   [index (- (first location) 1)])
               (if (= (length location) 1)
                 (substitute index '() mask)
                 (substitute index (rest location) mask))))

           (cond [(null? tree) tree]
                 [(null? location) (proc tree)]
                 [(eq? location '*) tree]
                 [else
                   (let ([location-mask (build-mask location (length (rest tree)))])
                     (pair (first tree) (map (curry tree-apply-proc proc) location-mask (rest tree))))]))
         ;;here pairs are lists of two items not scheme pairs
         (define (commutative-pair-equal pair1 pair2)
           (or (equal? pair1 pair2)
               (and (equal? (first pair1) (second pair2)) (equal? (second pair1) (first pair2)))))

         ;; there are ways to speed this up by preprocessing lst

         (define (unique-commutative-pairs lst func)
           (define (pairing-recursion lst1 lst2)
             (if (null? lst2)
               '()
               (let ((from1 (first lst1)))
                 (append (map (lambda (from2) (func from1 from2)) lst2)
                         (pairing-recursion (rest lst1) (rest lst2))))))
           (delete-duplicates (pairing-recursion lst (rest lst)) commutative-pair-equal))

         (define (list-unique-commutative-pairs lst)
           (unique-commutative-pairs lst list))

         ;;from standard-preamble.church
         ;; @desc
         ;; Compute the mean of a list of numbers.
         ;; @param lst The list. 
         ;; @returns number
         (define (my-mean lst) (/ (apply + lst) (length lst)))
         ;; @desc
         ;; Compute the variance of a list of numbers.
         ;; @param lst The list. 
         ;; @returns number
         (define (my-variance lst)
           (let ((mn (my-mean lst)))
             (my-mean (map (lambda (x) (expt (- x mn) 2)) lst))))

         ; Gaussian pdf, soft predicates
         (define (normal-pdf x mu sigma) (* (/ 1 (sqrt (* 2 3.1415 (expt sigma 2)))) (exp (- (/ (expt (- x mu) 2) (* 2 (expt sigma 2)))))))
         (define (normal-pdf-max sigma) (/ 1 (sqrt (* 2 3.1415 (expt sigma 2)))))
         (define (sigmoid s x) (/ 1.0 (+ 1 (exp (* (- x) s)))))
         (define (shift-fx t f) (lambda (x) (f (- x t))))
         (define (reflect-fx f) (lambda (x) (f (- x))))

         ; http://stackoverflow.com/questions/1869116/scheme-built-in-to-check-list-containment
         (define (contains? i l)
           (if (null? l) #f
             (or (equal? (first l) i) 
                 (contains? i (rest l)))))

         ; Conjunction of booleans
         (define (conj xs)
           (define (loop acc xs)
             (if (null? xs) acc
               (if (eq? #f acc) #f
                 (loop (and (first xs) acc) (rest xs)))))
           (loop #t xs))

         ; Disjunction
         (define (disj xs)
           (define (loop acc xs)
             (if (null? xs) acc
               (if (eq? #t acc) #t
                 (loop (or (first xs) acc) (rest xs)))))
           (loop #f xs))

         ; List group-by
         (define (group-by eqf f xs)
           (define idx-to-elts (make-hash-table eqf))
           (define (assign-group x)
             (let* ([fx (f x)])
               (if (hash-table-exists? idx-to-elts fx)
                 (hash-table-set! idx-to-elts fx (cons x (hash-table-ref idx-to-elts fx)))
                 (hash-table-set! idx-to-elts fx (list x)))))
           (begin 
             (for-each assign-group xs)
             (hash-table-values idx-to-elts)))

         ; List sort
         (define (sort cmp xs)
           (if (null? xs)
             '()
             (let*-values ([(pivot) (first xs)]
                           [(lt gt) (partition (lambda (x) (cmp x pivot)) (rest xs))])
                          (append (sort cmp lt) (list pivot) (sort cmp gt)))))

         (define (sort-by key cmp xs)
           (sort (lambda (x y) (cmp (key x) (key y))) xs))

         ; List median-split

         (define (median-split xs)
           (let* ([split-idx (div (length xs) 2)])
             (list (take xs split-idx) (drop xs split-idx))))

         ; List difference
         (define (list-subtract xs ys)
           (define (loop res xs)
             (cond [(null? xs) res]
                   [(contains? (first xs) ys) (loop res (rest xs))]
                   [else (loop (cons (first xs) res) (rest xs))]))
           (loop '() xs))

         ; Init
         (define (init xs)
           (take xs (- (length xs) 1)))

         (define (chop-last str)
           (list->string (init (string->list str))))

         (define (iterate n f x)
           (define (loop acc n)
             (if (= 0 n) acc
               (loop (f acc) (- n 1))))
           (loop x n))

         (define (list-remove-at idx xs)
           (define (iterate i acc xs)
             (cond [(null? xs) (reverse acc)]
                   [(= idx i) (iterate (+ i 1) acc (cdr xs))]
                   [else (iterate (+ i 1) (cons (car xs) acc) (cdr xs))]))
           (iterate 0 '() xs))

         (define (list-remove-at-several idxs xs)
           (define (iterate i acc xs)
             (cond [(null? xs) (reverse acc)]
                   [(contains? i idxs) (iterate (+ i 1) acc (cdr xs))]
                   [else (iterate (+ i 1) (cons (car xs) acc) (cdr xs))]))
           (iterate 0 '() xs))

         (define (list-idxs-where pred xs)
           (define (iterate acc i xs)
             (cond [(null? xs) (reverse acc)]
                   [(pred (car xs)) (iterate (cons i acc)
                                             (+ i 1)
                                             (cdr xs))]
                   [else (iterate acc
                                  (+ i 1)
                                  (cdr xs))]))
           (iterate '() 0 xs))

         (define (split-into n xs)
           (define k (round (/ (length xs) n)))
           (define (loop acc xs)
             (cond [(null? xs) acc]
                   [(< (length xs) k) (cons xs acc)]
                   [else (let*-values ([(head tail) (split-at xs k)])
                          (loop (cons head acc) tail))]))
           (if (>= n (length xs)) (map list xs)
             (reverse (loop '() xs))))
                   

         (define (ngram n xs)
           (define (look-ahead xs)
             (if (> n (length xs)) '()
               (iterate (- n 1) cdr xs)))
           (define (loop acc xs)
             (if (or (null? xs)
                     (null? (look-ahead xs))) acc
               (loop (append acc (list (take xs n))) (cdr xs))))
           (loop '() xs))

           (define (replace-car xs h)
             (cons h (cdr xs)))


         (define (sexp-walk f expr)
           (begin 
             (cond [(null? expr) expr]
                   [(list? expr) (let* ([new-expr (f expr)])
                                   (cond [(list? new-expr)
                                          (cons (sexp-walk f (car new-expr))
                                                (sexp-walk f (cdr new-expr)))]
                                         [else new-expr]))]
                   [(or (number? expr)
                        (string? expr)
                        (symbol? expr)) (f expr)]

                   [else expr])))

         (define (subexpr-walk f expr)
           (begin 
             (cond [(null? expr) expr]
                   [(list? expr) (let* ([new-expr (f expr)])
                                   (cond [(list? new-expr)
                                          (cons (subexpr-walk f (car new-expr))
                                                (subexpr-walk f (cdr new-expr)))]
                                         [else new-expr]))]
                   [else expr])))

         (define (sexpr-sort expr)
           (cond [(list? expr)
                  (sort default-compare
                        (map (lambda (e) (sexpr-sort e))
                             expr))]
                 [else expr]))
         (define (log-prob-sum2 . xs)
           (define (bin-log-prob x y)
             (+ y (log (+ 1 (exp (- x y))))))
           (fold bin-log-prob (car xs) (cdr xs)))

         (define (log-prob-sum . xs) ;; accounts for infinities.
           (cond [(null? xs) -inf.0]
                 [else
                   (log (fold (lambda (x y)
                                (+ (exp x) y))
                              (exp (car xs)) (cdr xs)))]))

         ;; From http://schemecookbook.org/Cookbook/StringSplit
         (define (str-split str ch)
           (let ((len (string-length str)))
             (letrec
               ((split
                  (lambda (a b)
                    (cond
                      ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
                      ((char=? ch (string-ref str b)) (if (= a b)
                                                        (split (+ 1 a) (+ 1 b))
                                                        (cons (substring str a b) (split b b))))
                      (else (split a (+ 1 b)))))))
               (split 0 0))))
         )

