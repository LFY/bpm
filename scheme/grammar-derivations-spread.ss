(library (grammar-derivations-spread)
    (export
      grammar-derivations
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

    (define (lazify-nts grammar+params)

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
                        `(abstraction ,(abstraction->name nt) ()
                            (lambda ()
                                (shift k 
                                    (list ,@(map 
                                        (lambda (param thunk) 
                                            `(list 
                                                ,(exp param)
                                                    (k ,thunk)
                                    )) params 
                                    choices)))))))
                (my-grammar->nts grammar+params) (grammar->params grammar+params)))

        `(program ,nts-with-params (TopLevel)))


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

    (define derivations '())

    (define (tree-walk f expr)
        (begin 
            (cond [(null? expr) (f expr)]
                  [(list? expr) (let* ([new-expr (f expr)])
                    (cond [(list? new-expr)
                        (cons (tree-walk f (car new-expr))
                        (tree-walk f (cdr new-expr)))]
                    [else new-expr]))]
            [else (f expr)]
    )))

    (define (partial? prob-tree)
        (reset 
            (begin (tree-walk (lambda (t) (cond [(procedure? t) (shift k #t)]
                                                [else t]))
                    prob-tree)
        #f)))

    (define (complete? prob-tree)
       (not (partial? prob-tree))
    )
    
    (define (pass? f prob-tree)
        (cond [(and (> (car prob-tree) prob-threshold) (f prob-tree)) #t]
            [else #f]))

    (define (probtree? tree)
        (number? (car tree)))

    (define (expand prob-partial-trees)
        (begin
            (set! derivations (append derivations (filter (lambda (prob-tree) (pass? complete? prob-tree)) prob-partial-trees)))
            (map
                (lambda (prob-partial-tree)
                    (let* ([curr-prob (car prob-partial-tree)]
                           [partial-tree (cadr prob-partial-tree)]
                           [next-trees-with-prob
                            (reset (tree-walk
                                        (lambda (t) (cond [(procedure? t) (t)]
                                                  [else t]))
                                    partial-tree))])
                    (list curr-prob next-trees-with-prob)))
            (filter (lambda (prob-tree) (pass? partial? prob-tree)) prob-partial-trees))))

    (define (compress layered-partial-trees)
        
        (define (no-inner-prob-trees? trees)
            (reset
                (begin
                    (map (lambda (prob-tree)
                        (let* ([tree (cadr prob-tree)])
                            (cond [(and (list? tree) (list? (car tree)) (probtree? (car tree)))
                                    (shift k #f)]
                                  [else tree])))
                    trees)
                #t)))


        (define (loop layered-partial-trees)
            (cond [(no-inner-prob-trees? layered-partial-trees) layered-partial-trees]
                  [else (loop (concatenate (map
                    (lambda (prob-tree1)
                        (let* ([prob (car prob-tree1)]
                               [trees (cadr prob-tree1)])
                                    (cond [(and (list? trees) (list? (car trees)) (probtree? (car trees)))
                                            (map (lambda (prob-tree2)
                                            (let* ([next-prob (car prob-tree2)]
                                                   [next-tree (cadr prob-tree2)])
                                            (list (* next-prob prob) next-tree))) trees)]
                                          [else (list (list prob trees))])))
                    layered-partial-trees)))]))
        
        (loop layered-partial-trees))

    (define (explore x) 
        (cond [(not (null? x)) (explore (compress (expand x)))]))
    
    (begin
        (let* 
            ([thunk-tree (eval (program->sexpr (lazify-nts grammar)) (environment '(rnrs) '(util) '(program) '(grammar-derivations-spread) '(delimcc-simple-ikarus) '(_srfi :1)))]
             [root-node (reset (thunk-tree))])
        (explore root-node))
        (sort (lambda (x y) (> (car x) (car y))) derivations)
    )

  )
)

