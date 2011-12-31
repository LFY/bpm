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

  (define (grammar-derivations grammar mode arg)

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

    (define (smallest-prob d)
        (caar (sort (lambda (x y) (< (car x) (car y))) d))
    )

    (define (cum-distribution d s)
        (cond [(not (null? d)) (cum-distribution (cdr d) (+ s (caar d)))] [else s]) 
    )

    (define (pass? f prob-tree)
        (cond [(= mode 0)
                (cond [(and (> (car prob-tree) arg) (f prob-tree)) #t]
                [else #f])]
              [(= mode 1)
                (cond 
                    [(and (f prob-tree) (or (< (length derivations) arg) (> (car prob-tree) (smallest-prob derivations)))) #t] 
                    [else #f])]
              [(= mode 2)
                (cond 
                    [(and (f prob-tree) (or (< (cum-distribution derivations 0) arg) (> (car prob-tree) (smallest-prob derivations)))) #t] 
                    [else #f])]
        )
    )

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
    
    (define (cum-distribution-list d num curr-total)
        (cond [(and (not (null? d)) (< curr-total arg)) (cum-distribution-list (cdr d) (+ 1 num) (+ (caar d) curr-total))]
            [else num]))

    (define (post-process d)
        (let* ([d-sorted (sort (lambda (x y) (> (car x) (car y))) d)])
        (cond [(= mode 0)
                d-sorted]
            [(= mode 1)
                (cond [(< (length d-sorted) arg) d-sorted]
                    [else (max-take d-sorted arg)])]
            [(= mode 2)
                (max-take d-sorted (cum-distribution-list d-sorted 0 0))]
        ))
    )

    (begin
        (let* 
            ([thunk-tree (eval (program->sexpr (lazify-nts grammar)) (environment '(rnrs) '(util) '(program) '(grammar-derivations-spread) '(delimcc-simple-ikarus) '(_srfi :1)))]
             [root-node (reset (thunk-tree))])
        (explore root-node))
        (post-process derivations)
    )

  )
)

