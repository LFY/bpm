(library (grammars)
         (export
           has-params?
           grammar->params
           nt->choices
           nt->name
           choice?
           grammar->nts
           grammar->tied-params
           grammar->stats
           grammar->history
           grammar-without-history

           grammar->start
          
           grammar-with-nts
           grammar-with-body
           grammar-with-params
           grammar-with-new-nts+body
           grammar-with-new-nts+body+history
           grammar-with-stats
          
           make-grammar
           compactify

           grammar-diff
           grammar->stat-vec
           )
         (import
           (rnrs)
           (program)
           (util)
           (_srfi :1))

         (define (grammar->stat-vec gr)
           (cdr (grammar->stats gr)))

         (define (grammar->posterior gr)
           (cadr (assoc 'posterior (grammar->stat-vec gr))))
           
         (define (grammar->likelihood gr)
           (let* ([like+weight (cdr (assoc 'likelihood+weight (grammar->stat-vec gr)))])
             (* (car like+weight) (cadr like+weight))))

         (define (grammar->prior gr)
           (let* ([prior+weight (cdr (assoc 'prior+weight (grammar->stat-vec gr)))])
             (* (car prior+weight) (cadr prior+weight))))

         (define (grammar->dl gr)
           (cadr (assoc 'desc-length (grammar->stat-vec gr))))

         (define (grammar-diff g2 g1)
           (let* ([g1-nts (grammar->nts g1)]
                  [g2-nts (grammar->nts g2)]
                  [nts-g1-not-g2 (lset-difference equal? g1-nts g2-nts)]
                  [nts-g2-not-g1 (lset-difference equal? g2-nts g1-nts)]
                  [delta-stats
                    `(delta-stats
                       (d-posterior ,(- (grammar->posterior g2) (grammar->posterior g1)))
                       (d-likelihood ,(- (grammar->likelihood g2) (grammar->likelihood g1)))
                       (d-prior ,(- (grammar->prior g2) (grammar->prior g1)))
                       (d-dl ,(- (grammar->dl g2) (grammar->dl g1))))]
                       )
             (list
               nts-g1-not-g2
               '==>
               nts-g2-not-g1
               (list 'delta-stats
                     delta-stats))))


         (define (make-grammar nts body . params)
           `(program
              ,nts
              ,body
              empty-params
              empty-stats
              (merge-history)
              ))

         (define (compactify-nt nt)
           (define (nt-call? expr)
             (and (list? expr) (symbol? (car expr))
                  (equal? (substring (symbol->string (car expr)) 0 1) "F")))
           (define (compactify-choice tree)
             (subexpr-walk
               (lambda (expr)
                 (let* ([cleaned (filter (lambda (x) (not (contains? x '(elem tr)))) expr)])
                   (cond [(nt-call? cleaned) (car cleaned)]
                         [else cleaned])))
               tree))
           (let* ([bodies (nt->choices nt)])
             `(,(abstraction->name nt) -> ,@(foldr1 (lambda (x y) (append (cond [(list? x) x]
                                                                               [else (list x)])
                                                                         (list '/) 
                                                                         (cond [(list? y) y]
                                                                               [else (list y)])))
                                             (map compactify-choice bodies)))))
         (define (compactify grammar)
           (grammar-with-new-nts+body
             grammar
             (map compactify-nt (grammar->nts grammar))
             (grammar->start grammar)))

         (define (grammar->history grammar)
           (append (cdr (list-ref grammar 5)) (list (grammar-without-history grammar))))

         (define (grammar->stats grammar)
           (list-ref grammar 4))

         (define (grammar->start grammar)
           (list-ref grammar 2))

         (define (grammar-with-nts grammar new-nts)
           (set-at 1 new-nts grammar))
         (define (grammar-with-body grammar new-body)
           (set-at 2 new-body grammar))
         (define (grammar-with-params grammar new-params)
           (set-at 3 new-params grammar))
         (define (grammar-with-new-nts+body grammar new-nts new-body)
           (set-at 2 new-body (set-at 1 new-nts grammar)))

         (define (grammar-with-stats grammar new-stats)
           (set-at 4 new-stats grammar))

         (define (grammar-without-history grammar)
           (set-at 5 '() grammar))

         (define (grammar-with-new-nts+body+history grammar new-nts new-body)
           (set-at 5 (append (list-ref grammar 5)
                             (list
                               (grammar-without-history grammar)))
                   (set-at 2 new-body (set-at 1 new-nts grammar))))
         
         (define (grammar->params grammar+params)
           (cadddr grammar+params))

         (define (has-params? grammar)
           (not (null? (cdddr grammar))))

         (define (grammar->nts grammar+params)
           (append (program->abstractions grammar+params)
                   (list `(abstraction TopLevel () ,(caddr (program->body grammar+params))))))

         (define nt->name abstraction->name)
         (define (choice? body) (eq? 'choose (car body)))
         (define (nt->choices nt)
           (let* ([main-body (abstraction->pattern nt)])
             (cond [(choice? main-body) (cdr main-body)]
                   [else (list main-body)])))

         (define (grammar->tied-params grammar+params)
           (define nts-with-params
             (map (lambda (nt params)
                    (let* ([choices (nt->choices nt)])
                      `(abstraction
                         ,(nt->name nt)
                         ()
                         (choose ,@(map (lambda (param thunk) `(list ,param ,thunk)) params (map (lambda (choice) `(lambda () ,choice)) choices))))))
                  (grammar->nts grammar+params) (grammar->params grammar+params)))
           `(program
              ,nts-with-params
              (lambda () (TopLevel))))

         )
