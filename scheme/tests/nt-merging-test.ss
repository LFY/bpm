(import (sxml2scfg)
        (beam-learning)
        (printing)
        (util)
        (program)
        (combinations)
        (_srfi :1)
        (program-likelihood)
        )

(define (nt->choices nt)
  (define (choice? body) (eq? 'choose (car body)))
  (let* ([main-body (abstraction->pattern nt)])
    (cond [(choice? main-body) (cdr main-body)]
          [else (list main-body)])))

(define (pairwise-nt-merges prog)
  (define (nt-pair->merge f1f2)
    (let* ([f1 (car f1f2)]
           [f2 (cadr f1f2)]
           [to-remove (map abstraction->name (list f1 f2))]
           [new-bodies (delete-duplicates (append (nt->choices f1) (nt->choices f2)))]
           [new-abstraction (begin (set-indices-floor! prog)
                                   (make-abstraction `(choose ,@new-bodies) '()))]
           ;; [new-abstraction (begin (set-indices-floor! prog)
                                   ;; (make-abstraction (if (= 1 (length new-bodies)) 
                                                       ;; (car new-bodies)
                                                       ;; `(choose ,@new-bodies)) '()))]

           [transform-expr (lambda (e) `(,(abstraction->name new-abstraction)))]
           [transform-pred (lambda (e) (and (non-empty-list? e) (contains? (car e) to-remove)))]
           [transform-pattern (lambda (e) (sexp-search transform-pred transform-expr e))]
           [transform-old-abstraction (lambda (a) (make-named-abstraction (abstraction->name a)
                                                                                      (transform-pattern (abstraction->pattern a))
                                                                                      (abstraction->vars a)))]
           [final-new-abstraction (transform-old-abstraction new-abstraction)]
           [new-program-body (transform-pattern (program->body prog))]
           [new-program-abstractions (filter (lambda (a) (not (contains? (abstraction->name a)
                                                                         to-remove)))
                                             (map transform-old-abstraction
                                                  (program->abstractions prog)))])
      (make-program (cons final-new-abstraction new-program-abstractions)
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

(define (gi-bmm data beam-size depth)
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
    (= 0 depth))

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
                                       beam-size depth
                                       program->transforms
                                       (lambda (progs) (batch-data-program->posterior data progs))
                                       (lambda (x) x)
                                       (same-prog-stop 5))])
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


(pretty-print (gi-bmm test-data 1 0))

