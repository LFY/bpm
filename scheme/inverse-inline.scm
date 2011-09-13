(library (inverse-inline)
         (export possible-abstractions 
                 replace-matches 
                 compressions 
                 condense-program
                 uniform-choice-compressions

                 get-all-subexpr-pairs
                 possible-typechecking-abstractions)
         (import (except (rnrs) string-hash string-ci-hash)
                 (program)
                 (_srfi :1)
                 (sym)
                 (unification)
                 (program)
                 (church readable-scheme)
                 (util)
                 (printing)
                 (combinations)
                 (hash-cons)
                 (printing))
         ;;return valid abstractions for any matching subexpressions in expr
         ;;valid abstractions are those without free variables
         (define (get-all-subexpr-pairs expr)
           (let* ([bimap (sexpr->dag expr)]
                  [num-subexpressions (bimap-size bimap)]
                  [subexpr-list (filter (lambda (i) (not (dag-primitive? i bimap))) (iota num-subexpressions))])
             (map (lambda (xy) (list (dag->sexpr-from (first xy) bimap)
                                     (dag->sexpr-from (second xy) bimap))) 
                  (append (map list subexpr-list subexpr-list)
                          (select-k-subsets 2 subexpr-list)))))        

         ;; old version:
         (define (get-all-subexpr-pairs-old expr)
           (select-k-subsets 2 (all-subexprs expr)))

         (define (possible-abstractions expr)
           (let* ([subexpr-pairs (get-all-subexpr-pairs expr)]
                  [abstractions (map-apply (curry anti-unify-abstraction expr) subexpr-pairs)])
             (filter-abstractions  abstractions)))

         (define (same-type-expression-pairs expr)
           (filter (lambda (e1e2) (eq? (car (car e1e2))
                                       (car (cadr e1e2))))
                     (get-all-subexpr-pairs expr)))

         (define (possible-typechecking-abstractions expr)
           (filter-abstractions (map-apply (curry anti-unify-abstraction expr) (same-type-expression-pairs expr))))

         (define (possible-typechecking-abstractions-with-arguments expr)
           (filter (lambda (a) (> (length (abstraction->vars a)) 0)) 
                   (filter-abstractions (map-apply (curry anti-unify-abstraction expr) (same-type-expression-pairs expr)))))

         ;;takes expr so that each abstraction can have the indices for function and variables set correctly
         ;;setting the indices floor only works if all functions in the program appear in expr, this is not the case if there are abstractions in the program that are not applied in the body 
         (define (anti-unify-abstraction expr expr1 expr2)
           (let* ([none (set-indices-floor! expr)]
                  [abstraction (apply make-abstraction (anti-unify expr1 expr2))]
                  ;; [none (reset-symbol-indizes!)]
                  )
             abstraction))

         ;;;remove undesirable abstractions and change any that have free variables
         (define (filter-abstractions abstractions)
           (define (remove-isomorphic abstractions)
             (delete-duplicates abstractions))
           (define (remove-nonmatches abstractions)
             (define (match? abstraction)
               (let* ([body (abstraction->pattern abstraction)])
                 (not (var? body))))
             (filter match? abstractions))
           (let* ([no-free-vars (map capture-free-variables abstractions)]
                  [no-isomorphisms (remove-isomorphic no-free-vars)]
                  [no-nonmatches (remove-nonmatches no-isomorphisms)])
             no-nonmatches))


         ;; doesn't deal with partial matches, could use more error checking;
         (define (replace-matches s abstraction)
           (let ([unified-vars (unify s
                                      (abstraction->pattern abstraction)
                                      (abstraction->vars abstraction))])
             (if (false? unified-vars)
               (if (list? s)
                 (map (lambda (si) (replace-matches si abstraction)) s)
                 s)
               (pair (abstraction->name abstraction)
                     (map (lambda (var) (replace-matches (rest (assq var unified-vars)) abstraction))
                          (abstraction->vars abstraction))))))

         (define (base-case? pattern var)
           (equal? (second (third pattern)) var))

         ;; throw out any matches that are #f

         (define (get-valid-abstractions subexpr-matches)
           (let ([abstractions (map third subexpr-matches)])
             (filter (lambda (x) x) abstractions)))

         (define (get/make-valid-abstractions subexpr-matches)
           (let* ([abstractions (map third subexpr-matches)]
                  [non-false (filter (lambda (x) x) abstractions)]
                  [no-free-vars (map capture-free-variables non-false)])
             no-free-vars))

         ;; joins definitions and program body into one big list
         (define (condense-program program)
           `(,@(map abstraction->pattern (program->abstractions program))
              ,(program->body program)))

         ;; both compressee and compressor are abstractions
         (define (compress-abstraction compressor compressee)
           (make-named-abstraction (abstraction->name compressee)
                                   (replace-matches (abstraction->pattern compressee) compressor)
                                   (abstraction->vars compressee)))

         (define (compress-program program abstraction)
           (let* ([compressed-abstractions (map (curry compress-abstraction abstraction)
                                                (program->abstractions program))]
                  [compressed-body (replace-matches (program->body program) abstraction)])
             (make-program (pair abstraction compressed-abstractions)
                           compressed-body)))


         ;; compute a list of compressed programs, nofilter is a flag that determines whether to return all compressions or just ones that shrink the program

         (define (filter-by-size p0 ps)
           (let* ([size0 (program-size p0)]
                  [valid-ps (filter (lambda (p) (< (program-size p) size0)) ps)])
             valid-ps))

         (define (mk-compression-transform abstr-gen transform-fx)
           (lambda (program . nofilter)
             (let* ([condensed-program (condense-program program)]
                    [abstractions (abstr-gen condensed-program)]
                    [compressed-programs (map (curry transform-fx program) abstractions)])
               (cond [(null? nofilter) (filter-by-size program compressed-programs)]
                     [else compressed-programs]))))

         (define (mk-compression-transform-without-abstr-gen abstr-pred transform-fx)
           (lambda (program . nofilter)
             (let* ([compressed-programs (map (curry transform-fx program) 
                                              (filter abstr-pred (program->abstractions program)))])
               (cond [(null? nofilter) (filter-by-size program compressed-programs)]
                     [else compressed-programs]))))


         (define compressions (mk-compression-transform possible-typechecking-abstractions compress-program))
         ;; (define uniform-choice-compressions (mk-compression-transform possible-typechecking-abstractions-with-arguments 
                                                                       ;; uniform-choice-compress-program))
        (define uniform-choice-compressions (mk-compression-transform-without-abstr-gen (lambda (abstr)
                                                                                          (> (length (abstraction->vars abstr)) 0))
                                                                                             uniform-choice-compress-program2))

(define (uniform-choice-compress-program2 prog abstr)
  (let* ([compressed-program prog]
         [latest-abstraction abstr]
         [arg-matrix-rows (append 
                            (apply zip (arg-matrix-in-abstractions compressed-program latest-abstraction))
                            (apply zip (arg-matrix compressed-program latest-abstraction)))]
         ;; [db (if (= 0 (length arg-matrix-rows))
         ;; (begin (print "Arg matrix has no rows.......")
         ;; (pretty-print abstr)))]
         [make-call-to-original (lambda (row) `(,(abstraction->name abstr) ,@row))]
         [new-abstraction (make-abstraction `(choose ,@(map make-call-to-original arg-matrix-rows)) '())]
         [transform-expr (lambda (e) `(,(abstraction->name new-abstraction)))]
         [transform-pred (lambda (e) (and (non-empty-list? e) (eq? (car e) (abstraction->name abstr))))]
         [transform-pattern (lambda (e) (sexp-search transform-pred transform-expr e))]
         [new-program-body (transform-pattern (program->body compressed-program))]
         [new-program-abstractions (map (lambda (a) (make-named-abstraction (abstraction->name a)
                                                                            (transform-pattern (abstraction->pattern a))
                                                                            (abstraction->vars a)))
                                        (program->abstractions compressed-program))])
    (make-program (cons new-abstraction new-program-abstractions)
                  new-program-body)))

         (define (uniform-choice-compress-program prog abstr)
           (let* ([compressed-program (compress-program prog abstr)]
                  [latest-abstraction (car (program->abstractions compressed-program))]
                  [arg-matrix-rows (append 
                                     (apply zip (arg-matrix-in-abstractions compressed-program latest-abstraction))
                                     (apply zip (arg-matrix compressed-program latest-abstraction)))]
                  ;; [db (if (= 0 (length arg-matrix-rows))
                        ;; (begin (print "Arg matrix has no rows.......")
                               ;; (pretty-print abstr)))]
                  [make-call-to-original (lambda (row) `(,(abstraction->name abstr) ,@row))]
                  [new-abstraction (make-abstraction `(choose ,@(map make-call-to-original arg-matrix-rows)) '())]
                  [transform-expr (lambda (e) `(,(abstraction->name new-abstraction)))]
                  [transform-pred (lambda (e) (and (non-empty-list? e) (eq? (car e) (abstraction->name abstr))))]
                  [transform-pattern (lambda (e) (sexp-search transform-pred transform-expr e))]
                  [new-program-body (transform-pattern (program->body compressed-program))]
                  [new-program-abstractions (map (lambda (a) (make-named-abstraction (abstraction->name a)
                                                                                     (transform-pattern (abstraction->pattern a))
                                                                                     (abstraction->vars a)))
                                                 (program->abstractions compressed-program))])
             (make-program (cons new-abstraction new-program-abstractions)
                           new-program-body)))
)
