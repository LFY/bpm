(library
  (grammar-proposals)
  (export
    init-grammar
    split-merge-proposal
    sample-split
    multiple-split-merge-proposal
    sample-merge
    
    same-type-predicate
    all-nts-compatible
    page-root-compatible
    
    nt-name->users
    nt->successors
    to-mgcg
    )

  (import (rnrs) (grammars)
          (ikarus)
          (grammar-induction)
          (program)
          (grammar-likelihood)
          (chart-parsing)
          (util)
          (delimcc-simple-ikarus)
          (_srfi :1)
          (sym))

  ;; Split proposal ==============================================================

  (define (has-app? nt-name body)
    (reset
      (begin
        (subexpr-walk (lambda (t)
                        (cond
                          [(equal? nt-name (car t)) (shift k #t)]
                          [else t])) body)
        #f)))

  (define (nt-name->users grammar nt-name)
    (define (nt-uses? name)
      (lambda (nt)
        (let* ([choices (nt->choices nt)])
          (reset
            (begin
              (for-each (lambda (choice) (cond [(has-app? nt-name choice) (shift k #t)]
                                               [else #f]))
                        choices)
              #f)))))
    (let* ([used-in (filter (nt-uses? nt-name) (grammar->nts grammar))])
      used-in))

  ;; nested lists of functions, one list per choice, of the format (successor-name k),
  ;; where k is a 1-argument function that can be used to set the successor, _mutating_ the original NT

  (define (nt->successors nt)
    (define (choice->successor-mutators choice)
      (define successor-mutators '())
      (subexpr-walk
        (lambda (t)
          (cond [(and (= (length t) 1) (symbol? (car t)) (equal? "F" (substring (symbol->string (car t)) 0 1)))
                 (let* ([mutator (lambda (new-successor)
                                   (set-car! t new-successor))])
                   (begin
                     (set! successor-mutators
                       (cons (list (car t) mutator)
                             successor-mutators))) )]
                [else t]))
        choice)
      successor-mutators)
    (let* ([choices (nt->choices nt)])
      (map choice->successor-mutators choices)))

  (define (set-successor-at! grammar nt-name choice-idx succ-idx new-succ)
    (let* ([successors (nt->successors (car (filter (lambda (nt) (equal? nt-name (nt->name nt)))
                                                    (grammar->nts grammar))))]
           [nt-mutator (cadr (list-ref
                               (list-ref (nt->successors (car (filter (lambda (nt) (equal? nt-name (nt->name nt)))
                                                                      (grammar->nts grammar))))
                                         choice-idx) succ-idx))])
      (nt-mutator new-succ)))

  (define (grammar-copy gr)
    (subexpr-walk (lambda (t) t) gr))


  (define (rndflip)
    (if (= 1 (random-from-range 0 1)) #t #f))

  (define (uniform-sample-proper-subset xs)
    (define (loop result)
      (cond [(not (= (length result) (length xs))) result]
            [else (loop (filter rndflip result))]))
    (loop xs))

  (define (sample-nontrivial-bool-vector n)
    (define (loop result)
      (cond [(or (conj result)
                 (not (disj result))) (loop (map (lambda (x) (rndflip)) (iota n)))]
            [else result]))
    (loop (iota n)))

  (define (grammar->body grammar)
    (program->body grammar))

  (define (grammar-replace-nt grammar new-nt nt-name)
    (map (lambda (nt) (cond [(equal? nt-name (nt->name nt)) new-nt]
                            [else nt]))
         (grammar->nts grammar)))

  (define (grammar-get-nt grammar nt-name)
    (car (filter (lambda (nt) (equal? nt-name (nt->name nt))) (grammar->nts grammar))))

  (define (all-splittable-nts grammar)
    (define (splittable? nt)
      (> (length (nt-name->users grammar (nt->name nt))) 1))
    (filter splittable? (grammar->nts grammar)))

  (define (sample-splittable-nt grammar)
    (define (splittable? nt)
      (> (length (nt-name->users grammar (nt->name nt))) 1))
    (uniform-select (filter splittable? (grammar->nts grammar))))

  (define (all-places-used grammar nt-name)
    (filter (lambda (x) (equal? nt-name (car x))) 
            (concatenate 
              (map (lambda (n) (concatenate (nt->successors n))) 
                   (nt-name->users grammar nt-name)))))

  (define (get-split grammar nt-name)
    (let* ([void (set-indices-floor! grammar)]
           [split-names (list (sym (func-symbol)) (sym (func-symbol)))]
           [my-mutators (filter (lambda (x) (equal? nt-name (car x))) 
                                (concatenate 
                                  (map (lambda (n) (concatenate (nt->successors n))) 
                                       (nt-name->users grammar nt-name))))]
           [split-distr (map (lambda (b)
                               (if b (car split-names) (cadr split-names))) 
                             (sample-nontrivial-bool-vector (length my-mutators)))]
           [mutate-and-copy
             (lambda (mutator new-succ-name)
               (begin
                 ((cadr mutator) new-succ-name)
                 (grammar-copy grammar)))]
           [void
             (map (lambda (mutator name)
                    ((cadr mutator) name))
                  my-mutators split-distr)]
           [all-choices (nt->choices (grammar-get-nt grammar nt-name))]
           [choice-selections (zip all-choices (map (lambda (i) (random-from-range 0 2)) (iota (length all-choices))))]
           [fsplit1-choices all-choices]
           [fsplit2-choices all-choices]
           [fsplit1-def
             (make-named-abstraction
               (car split-names)
               (cond [(= (length fsplit1-choices) 1)
                      (car fsplit1-choices)]
                     [else
                       `(choose ,@fsplit1-choices)])
               '())]
           [fsplit2-def
             (make-named-abstraction
               (cadr split-names)
               (cond [(= (length fsplit2-choices) 1)
                      (car fsplit2-choices)]
                     [else
                       `(choose ,@fsplit2-choices)])
               '())]
           [new-nts (append (list fsplit1-def fsplit2-def)
                            (filter (lambda (nt) (not (equal? nt-name (nt->name nt))))
                                    (program->abstractions grammar)))])
      (grammar-with-new-nts+body
        grammar
        new-nts
        (grammar->start grammar))))

  (define (chart->node-defs chart)
    (cadr chart))

  (define (node-def->rule-idx node-def)
    (caddr (cadr node-def)))

  (define (has-prefix? lhs-sym)
    (let* ([name-str (symbol->string lhs-sym)]
           [split_underscore (str-split name-str #\_)])
      (<= 3 (length split_underscore))))

  (define (remove-prefix name)
    (if (has-prefix? name)
      (let* ([name-str (symbol->string name)]
             [split_underscore (str-split name-str #\_)])
        (string->symbol (caddr split_underscore)))
      name))

  (define (uses-nt? nt-name def)
    (equal? (remove-prefix (cadr (cadr def))) nt-name))

  (define (grammar-with-new-nts+body2 grammar nts-with-start)
    (let* ([non-start (filter (lambda (nt) (not (equal? 'TopLevel (nt->name nt)))) nts-with-start)]
           [body (car (filter (lambda (nt) (equal? 'TopLevel (nt->name nt))) nts-with-start))])
      (grammar-with-new-nts+body
        grammar
        non-start
        `(lambda () (choose ,@(nt->choices body))))))

  (define (prune-extraneous-rules grammar charts)

    (define (find-used-rule-idxs nt-name chart)
      (let* ([node-defs (chart->node-defs chart)])
        (delete-duplicates (map node-def->rule-idx (filter (lambda (def) (uses-nt? nt-name def)) node-defs)))))

    (define (find-all-used-rule-idxs nt-name charts)
      (delete-duplicates (concatenate (map (lambda (chart) (find-used-rule-idxs nt-name chart)) charts))))

    (let* ([used-rule-idxs (map (lambda (nt) (find-all-used-rule-idxs (nt->name nt) charts))
                                (grammar->nts grammar))]
           [new-nts (map (lambda (used-rules nt)
                           (make-named-abstraction
                             (nt->name nt)
                             (cond
                               [(= (length used-rules) 1)
                                (list-ref (nt->choices nt) (car used-rules))]
                               [else
                                 `(choose ,@(map (lambda (idx) (list-ref (nt->choices nt) idx)) used-rules))])
                             '()))
                         used-rule-idxs (grammar->nts grammar))])
      (grammar-with-new-nts+body2
        grammar
        new-nts)))

  (define (clean-grammar data new-split)
    (let* ([charts (car (batch-run-inversion (list new-split) data))])
      (prune-extraneous-rules new-split charts)))
           

  (define (sample-split data grammar mergeable? like-weight prior-weight prior-parameter)
    (define gr (grammar-copy grammar))
    (if (null? (all-splittable-nts gr)) 
      (let* ([score-charts-gr (grammar->posterior+charts+grammar data gr like-weight prior-weight prior-parameter)])
        (list 0.0 (car score-charts-gr) (caddr score-charts-gr)))
      (let* ([num-splittable (length (all-splittable-nts gr))]
             [to-split (sample-splittable-nt gr)]
             [num-uses-to-split (length (all-places-used gr (nt->name to-split)))]
             [possible-ways (expt 2 (- num-uses-to-split 1))]
             [new-gr (clean-grammar data (get-split gr (nt->name to-split)))]
             [new-gr+stats+score+charts (grammar->posterior+charts+grammar data new-gr like-weight prior-weight prior-parameter)]
             [new-score (car new-gr+stats+score+charts)]
             [new-charts (cadr new-gr+stats+score+charts)]
             [new-gr+stats (caddr new-gr+stats+score+charts)]
             ;; backward prob calc
             ;; the reverse merge probability is then taking the new grammar, sampling the number of mergeable nt's, and then sampling the other nt's of that type. so probably something that is best tried on the new grammar
             [mergeable (all-mergeable-nts new-gr mergeable?)]
             [num-mergeable-nts (length mergeable)]
             [num-mergeable-nt2s (length (filter (lambda (nt) (mergeable? to-split nt))
                                                 (nts-other-than to-split mergeable)))]
             [bwd-prob (+ (- (log num-mergeable-nts)) (- (log num-mergeable-nt2s)))]
             [fwd-prob (+ (- (log num-splittable)) (- (log possible-ways)))]
             )
        (list (- bwd-prob fwd-prob) new-score new-gr+stats))))

  ;; Merge proposal ==============================================================

  (define (nt->type nt)
    (cadr (car (nt->choices nt))))

  (define (filter-by-type t nts)
    (filter (lambda (nt)
              (equal? t (nt->type nt)))
            nts))

  (define (nts-other-than nt1 nts)
    (filter (lambda (nt) (not (equal? (nt->name nt)
                                      (nt->name nt1))))
            nts))

  ;; (define (same-type-predicate all-nts nt)
  ;;   (let* ([t (nt->type nt)])
  ;;     (> (length (filter-by-type t all-nts)) 1)))
  (define (same-type-predicate nt1 nt2)
    (equal? (nt->type nt1) (nt->type nt2)))

  (define (all-nts-compatible all-nts nt) #t)

  (define (page-root-compatible nt1 nt2)
    (define (nt->type nt)
      (caar (nt->choices nt)))
    (if (or
          (equal? 'page_root (nt->type nt1))
          (equal? 'page_root (nt->type nt2)))
      (equal? (nt->type nt1) (nt->type nt2))
      #t))

  (define (all-mergeable-nts grammar mergeable?)
    (let* ([all-nts (filter (lambda (nt) (not (equal? 'TopLevel (nt->name nt))))
                            (grammar->nts grammar))])
      all-nts))

  (define (rnd-select-mergeable-pair grammar mergeable?)
;; step 1: select some NT that can be merged
    ;; step 2: select a different NT with same type. if we can't do this, return '()
    (let* ([mergeable (filter (lambda (nt) (not (equal? 'TopLevel (nt->name nt)))) (grammar->nts grammar))])
      (cond [(null? mergeable) '()]
            [else
              (let* ([nt1 (uniform-select mergeable)]
                     [nt2-candidates (filter (lambda (nt) (mergeable? nt1 nt))
                                             (nts-other-than nt1 mergeable))])
                (cond [(null? nt2-candidates) '()]
                      [else
                        (let* ([nt2 (uniform-select nt2-candidates)]
                               ;; fwd prob: pick the first mergeable nt,
                               ;; then the second.
                               [fwd-prob (+ (- (log (length mergeable))) (- (log (length nt2-candidates))))])
                          (list
                            fwd-prob
                            (list nt1 nt2)
                            ))]))])))


  (define (remove-duplicate-choices grammar)
    (define (remove-duplicates-for-nt nt)
      (let* ([new-choices (delete-duplicates (nt->choices nt))])
        (cond [(= 1 (length new-choices))
               (make-named-abstraction
                 (abstraction->name nt)
                 (car new-choices)
                 '())]
              [else
                (make-named-abstraction
                  (abstraction->name nt)
                  `(choose ,@new-choices)
                  '())])))

    (define (remove-top-level-duplicates thunk)
      (let* ([new-choices
               (delete-duplicates (cdr (caddr thunk)))])
        `(lambda () (choose ,@new-choices))))
    (grammar-with-new-nts+body
      grammar
      (map remove-duplicates-for-nt (program->abstractions grammar))
      (remove-top-level-duplicates (program->body grammar))
      ))

  (define 
    (merge-nts prog f1f2)
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
           [new-abstraction (make-named-abstraction 
                              new-abstraction-name 
                              (cond [(= 1 (length new-bodies)) (car new-bodies)] 
                                    [else `(choose ,@new-bodies)])
                              '())]
           )

      (remove-duplicate-choices
        (grammar-with-new-nts+body
          prog
          (cons new-abstraction
                new-program-abstractions)
          new-program-body)
        )
      ))

  (define (to-mgcg data grammar mergeable? like-weight prior-weight prior-parameter fx)
    (let* ([next (sample-merge data grammar mergeable? like-weight prior-weight prior-parameter)]
           [void (fx next)])
      (to-mgcg data (caddr next) mergeable? like-weight prior-weight prior-parameter fx)))

  (define (sample-merge data grammar mergeable? like-weight prior-weight prior-parameter)
    (define gr (grammar-copy grammar))
    (let* ([fwd-prob-mergeable-candidates (rnd-select-mergeable-pair gr mergeable?)])
      (if (null? fwd-prob-mergeable-candidates) 
        
        (let* ([score-charts-gr (grammar->posterior+charts+grammar data gr like-weight prior-weight prior-parameter)])
          (list 0.0 (car score-charts-gr) (caddr score-charts-gr)))
        (let* 
          ([mergeable-candidates (cadr fwd-prob-mergeable-candidates)]
           [fwd-prob (car fwd-prob-mergeable-candidates)]
           [merged-grammar (merge-nts gr mergeable-candidates)]
           [gr+stats+score+charts (grammar->posterior+charts+grammar data merged-grammar like-weight prior-weight prior-parameter)]
           [new-gr-score (car gr+stats+score+charts)]
           [new-charts (cadr gr+stats+score+charts)]
           [new-gr+stats (caddr gr+stats+score+charts)]
           [new-nt-name (nt->name (car (program->abstractions merged-grammar)))]
           [new-incoming-size (length (all-places-used merged-grammar new-nt-name))]
           [num-splittable (length (all-splittable-nts merged-grammar))]
           [num-possible-splits (expt 2 (- new-incoming-size 1))]
           [bwd-prob (+ (- (log num-splittable)) (- (log num-possible-splits)))]
           )
          (list (- bwd-prob fwd-prob) new-gr-score new-gr+stats)))))

  (define (init-grammar data)
    (populate-stats data (assign-uniform-params (lgcg data))))

  (define (grammar->score prior-parameter data grammar)
    (let* ([prior (grammar-prior prior-parameter grammar)]
           [likelihood (single-data-grammar->likelihood data grammar)])
      (+ prior likelihood)))

  (define (split-merge-proposal data mergeable? likelihood-weight prior-weight prior-parameter)
    (lambda (gr)
      (let* ([prop-fx (uniform-select (list (lambda (g) (sample-split data g mergeable? likelihood-weight prior-weight prior-parameter))
                                            (lambda (g) (sample-merge data g mergeable? likelihood-weight prior-weight prior-parameter))))])
        (prop-fx gr))))

  (define (multiple-split-merge-proposal depth data)
    (lambda (gr)
      (letrec ([loop (lambda (n correction-gr)
                       (let* ([correction (car correction-gr)]
                              [gr (cadr correction-gr)]
                              [fx (uniform-select (list sample-merge (lambda (g) (sample-split data g))))]
                              [next-correction-gr (fx gr)]
                              [next-correction (car next-correction-gr)]
                              [next-gr (cadr next-correction-gr)]
                              [next-iter-correction-gr (list (+ next-correction correction) next-gr)]
                              )
                         (cond [(= n 1) next-iter-correction-gr]
                               [else (loop (- n 1) next-iter-correction-gr)])))])
        (loop depth (list 0.0 gr)))))
  )
