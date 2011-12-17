(library (grammar-derivations-spread)
    (export
      grammar-derivations
      reify0
      elem
      tr
    )
    (import (except (rnrs) string-hash string-ci-hash)
        (rnrs eval)
        (util)
        (_srfi :1)
        (_srfi :69)
        (printing)
        (program)
        (delimcc-simple-ikarus)
    )

    (define (tie-parameters-to-choices grammar+params)

        (define (grammar->params grammar+params)
            (cadddr grammar+params))

        (define (my-grammar->nts grammar+params)
            (append (program->abstractions grammar+params)
            (list `(abstraction TopLevel () ,(caddr (program->body grammar+params))))))

        (define nts-with-params
            (map (lambda (nt params)
                    (let* ([choices (cond [(eq? 'choose (car (abstraction->pattern nt))) 
                        (cdr (abstraction->pattern nt))]
                        [else (list (abstraction->pattern nt))])])
        `(abstraction ,(abstraction->name nt)()
            (reify0 (lambda () (shift k (list ,@(map (lambda (param thunk) `(list ,(exp param) ,thunk)) params (map (lambda (choice) `(lambda () , `(k, choice))) choices)))))))))
            (my-grammar->nts grammar+params) (grammar->params grammar+params)))

        `(program ,nts-with-params (lambda () (TopLevel))))

    (define-syntax define-constr
        (syntax-rules ()
        [(define-constr name)
            (define (name . xs)
            (cons 'name xs))]))

    (define-constr elem)
    (define-constr tr)
    
    (define (reify0 thunk)
        (reset (thunk)))

(define (grammar-derivations grammar prob-threshold)
    (define bfs-queue '())
    (define derivations '())
    (define formatted-derivations '())
    (define (add-to-bfs-queue prob node-trace thunk) (set! bfs-queue (append bfs-queue (list (list prob node-trace thunk)))))
    (define (add-to-derivations prob node-trace) (set! derivations (sort (lambda (x y) (> (car x) (car y))) (append derivations (list (list prob node-trace))))))
        
    (define (create-trace prob node-trace search-node)
      (cond
        [(and (not (null? search-node)) (> prob prob-threshold))
         (let* ([val-or-thunk (car search-node)])
           (begin
             ;;(pretty-print val-or-thunk)
             ;;(pretty-print derivations)
             (cond [(number? val-or-thunk) 
                    (create-trace (* prob val-or-thunk) node-trace (cdr search-node))]

                   [(list? val-or-thunk) 
                    (begin 
                      (create-trace prob node-trace val-or-thunk) 
                      (create-trace prob node-trace (cdr search-node)))]

                   [(procedure? val-or-thunk) 
                    (add-to-bfs-queue prob node-trace val-or-thunk)]

                   [(cond 
                      [(null? (cddr search-node)) 
                       (add-to-derivations prob (append node-trace (list val-or-thunk (cadr search-node))))]
                      [else 
                        (create-trace prob 
                                      (append node-trace (list val-or-thunk (cadr search-node))) 
                                      (cddr search-node))])])))])
      )

    (define (bfs-search)
        (cond [(not (null? bfs-queue))
        (let* ([bfs-entry (car bfs-queue)]
            [prob (car bfs-entry)]
            [node-trace (cadr bfs-entry)]
            [thunk (caddr bfs-entry)])
            (begin
                (set! bfs-queue (cdr bfs-queue))
                (create-trace prob node-trace (thunk))
                (bfs-search)))])
    )

    (define (formatting d)
        (let* ( [derivation (cadr d)]
                [formatted-derivation (add-parens derivation)])
            (set! formatted-derivations (append formatted-derivations (list (list (car d) formatted-derivation)))))
    )
    
    (define (add-parens derivation)
        (cond [(not (null? derivation)) 
            (let* ([first-two (list (car derivation) (cadr derivation))])
            (cond [(not (null? (cddr derivation))) (reverse (cons (add-parens (cddr derivation)) (reverse first-two)))]
                  [else first-two]))]
        )
    )
    
    (begin
        (let* 
            ([thunk-tree (eval (program->sexpr (tie-parameters-to-choices grammar)) (environment '(rnrs) '(util) '(program) '(grammar-derivations-spread) '(delimcc-simple-ikarus) '(_srfi :1)))]
             [root-node (reify0 thunk-tree)])
            (begin
                (add-to-bfs-queue 1.0 '() (cadar root-node)) 
                (bfs-search)
                (map formatting derivations)
                formatted-derivations
            )))
)

)
