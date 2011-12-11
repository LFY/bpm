(library (grammars)
         (export
           has-params?
           grammar->params
           nt->choices
           nt->name
           choice?
           grammar->nts
           grammar->tied-params
          
           grammar-with-nts
           grammar-with-body
           grammar-with-params
           grammar-with-new-nts+body
           grammar-with-new-nts+body+history
           grammar-with-stats
          
           make-grammar
           )
         (import
           (rnrs)
           (program)
           (util)
           (_srfi :1))

         (define (make-grammar nts body . params)
           `(program
              ,nts
              ,body
              empty-params
              empty-stats
              (merge-history)
              ))

         (define (grammar->history grammar)
           (list-ref grammar 5))

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
           (set-at 5 (append (grammar->history grammar) 
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
