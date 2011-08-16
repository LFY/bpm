#!r6rs
;;TO DO
;;-adjust tree-apply-proc to not be dependent on * as a masking character
;;-use data abstraction for location in tree-apply-proc
(library (util)
         (export all-equal? all-assoc curry all max-take sexp-replace sexp-search get/make-alist-entry rest pair random-from-range depth tree-apply-proc primitive? non-empty-list? all-subexprs deep-find-all map-apply more-than-one primitives list-unique-commutative-pairs unique-commutative-pairs my-mean my-variance thunkify normal-pdf deep-find display-all tagged-list? list-or are-all

                 println

                 shallow-find-all
                 sexp-replace-pred

                 contains?
                 group-by
                 sort
                 median-split
                 list-subtract
                 init
                 iterate
                 ngram

                 conj

                 normal-pdf-max
                 sigmoid
                 shift-fx
                 reflect-fx

                 fcomp
                 compose
                 )
         (import (except (rnrs) string-hash string-ci-hash)
                 (printing)
                 (_srfi :1)
                 (_srfi :69)
                 (church readable-scheme))


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

         (define (iterate n f x)
           (define (loop acc n)
             (if (= 0 n) acc
               (loop (f acc) (- n 1))))
           (loop x n))

         (define (ngram n xs)
           (define (look-ahead xs)
             (if (> n (length xs)) '()
               (iterate (- n 1) cdr xs)))
           (define (loop acc xs)
             (if (or (null? xs)
                     (null? (look-ahead xs))) acc
               (loop (append acc (list (take xs n))) (cdr xs))))
           (loop '() xs))
         )

