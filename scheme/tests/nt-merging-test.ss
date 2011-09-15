(import (sxml2scfg)
        (beam-learning)
        (printing)
        (util)
        (program)
        (combinations)
        (_srfi :1)
        (_srfi :69)
        (program-likelihood)
        (sym)
        )

(define (nt->choices nt)
  (define (choice? body) (eq? 'choose (car body)))
  (let* ([main-body (abstraction->pattern nt)])
    (cond [(choice? main-body) (cdr main-body)]
          [else (list main-body)])))

(define (pairwise-nt-merges prog)
  (define (nt-pair->merge f1f2)
    (let* ([none (set-indices-floor! prog)]

           [new-abstraction-name (sym (func-symbol))]

           [to-remove (map abstraction->name f1f2)]

           [transform-pattern (let* (
                                     [transform-expr (lambda (e) `(,new-abstraction-name))]
                                     [transform-pred (lambda (e) (and (non-empty-list? e) (contains? (car e) to-remove)))])
                                (lambda (e) (sexp-search transform-pred transform-expr e))
                                )]

           [transform-old-abstraction (lambda (a) (make-named-abstraction (abstraction->name a)
                                                                          (transform-pattern (abstraction->pattern a))
                                                                          (abstraction->vars a)))]

           [new-program-body (transform-pattern (program->body prog))]
           [new-program-abstractions (filter (lambda (a) (not (contains? (abstraction->name a) to-remove)))
                                             (map transform-old-abstraction (program->abstractions prog)))]

           [f1f2* (map transform-old-abstraction f1f2)]

           [new-bodies (delete-duplicates (append (nt->choices (first f1f2*)) (nt->choices (second f1f2*))))]

           [new-abstraction (make-named-abstraction new-abstraction-name
                                                    `(choose ,@new-bodies)
                                                    '())]
           )

      (make-program (cons new-abstraction
                          new-program-abstractions)
                    new-program-body)))

  (define (same-type? f1f2)
    (eq? (car (car (nt->choices (car f1f2))))
         (car (car (nt->choices (cadr f1f2))))))

  (let* ([possible-merges (map nt-pair->merge
                               (filter same-type? 
                                       (select-k-subsets 2 (program->abstractions prog))))])
    possible-merges))

(define (elt? sym) 
  (and (symbol? sym)
       (equal? "e" (substring (symbol->string sym) 0 1))))

(define elt-pred (lambda (e) (and (not (null? e)) 
                                  (list? e) 
                                  (not (null? (cdr e))) 
                                  (elt? (car e)))))

(define (gi-bmm data beam-size . stop-at-depth)

  (define prog-table (make-hash-table equal?))

  (define (program->exists? prog likelihood)
    (define (prog->unlabeled prog)
      (define (abstr->num-successors prog abstr)
        (let* ([is-successor? (lambda (expr)
                                (and (non-empty-list? expr)
                                     (= 1 (length expr))
                                     (contains? (car expr) (map abstraction->name (program->abstractions prog)))))])
          (length (sexp-search is-successor? (lambda (x) x) (abstraction->pattern abstr)))))
      (map (curry abstr->num-successors prog) (program->abstractions prog)))
    (let* ([hash (list likelihood (prog->unlabeled prog))])
      (if (hash-table-exists? prog-table hash) #t
        (begin (hash-table-set! prog-table hash prog) #f))))

  (define (fringe->merged-fringe prog-likelihoods) ;; can result in starvation of the beam
    (define (iterator prog-likelihoods new-fringe)
      (cond [(null? prog-likelihoods) new-fringe]
            [else 
              (let* ([prog-likelihood (car prog-likelihoods)]
                     [prog (car prog-likelihood)]
                     [likelihood (cadr prog-likelihood)])
                (begin (if (program->exists? prog likelihood)
                         (iterator (cdr prog-likelihoods) new-fringe)
                         (iterator (cdr prog-likelihoods) (cons prog-likelihood new-fringe)))))]))
    (iterator prog-likelihoods '()))

  (define (program->transforms prog)
    (begin
      (cons prog (pairwise-nt-merges prog)
            )))

  (define (program->log-posterior prog)
    (apply + (map (lambda (d) (data-program->log-posterior d prog))
                  data)))
  (define (print-stats fringe depth)
    (let ([best-prog (car fringe)])
      (begin (print "depth: ~s best program:" depth)
             (print "posterior: ~s" (program->log-posterior best-prog))
             (pretty-print (car fringe)))))


  (define (depth-stop fringe depth)
    (begin (print-stats fringe depth)
           (= 0 depth)))

  (define (same-prog-stop limit)
    (define prog-store '())
    (define (add-one-prog p) (set! prog-store (cons p prog-store)))
    (define (reached-limit?)
      (let ([tail (max-take prog-store limit)])
        (cond [(>= (length tail) limit) (= 1 (length (delete-duplicates tail)))]
              [else #f])))

    (lambda (fringe depth)
      (begin (print-stats fringe depth)
             (add-one-prog (car fringe))
             (reached-limit?))))

  (let* ([initial-prog (sxmls->initial-program elt-pred data)]
         [learned-program (beam-search-batch-score (list initial-prog)
                                       beam-size (if (not (null? stop-at-depth)) (car stop-at-depth) 0)
                                       program->transforms
                                       (lambda (progs) (batch-data-program->posterior data progs))
                                       fringe->merged-fringe
                                       (if (not (null? stop-at-depth)) depth-stop (same-prog-stop 10)))])
    learned-program))


;; simple recursion, cf. the toy example @ beginning of GI paper
(define test-data 
  (list 
    '(er (my5 (er (my5 (er (my5 (er)))))))
    '(er (my5 (er (my5 (er)))))
    '(er (my5 (er)))
    '(eb (my5 (eb (my5 (eb (my5 (eb)))))))
    '(eb (my5 (eb (my5 (eb)))))
    '(eb (my5 (eb)))
    ))

(pretty-print (gi-bmm test-data 1))

