(import (rnrs)
        (_srfi :1)
        (printing)
        (util)
        (chart-parsing)
        (program)
        (named-search-trees))

(define (mk-prog-body p) (list 'lambda '() p))
(define test1 (make-program 
                       (list 
                         (make-named-abstraction 'f1 '(node (data (a0 v0) 
                                                                  (a1 v1) 
                                                                  (a2 v2)
                                                                  (a3 v3)
                                                                  )
                                                            (nondet-choice v0 v1)
                                                            
                                                            ) '(v0 v1 v2 v3))) 
                       (mk-prog-body '(node (data (a0 111)) 
                                            (f1 1 -1 2 -2) 
                                            (f1 2 -2 3 -3)
                                            (f1 4 -4 5 -5)
                                            (f1 7 -7 8 -8)
                                            (f1 90 -90 91 -91)))
                       ))

(define (parse-tree->prob tree)
  (cond [(eq? 'tree (car tree)) (let* ([subtrees (cddddr tree)]
                                       [my-prob (/ 1 (cadddr tree))])
                                  (* my-prob 
                                     (apply * (map parse-tree->prob subtrees))))]
        [(list? (car tree)) (apply + (map parse-tree->prob tree))]
        [else 1]))

(define test-scfg '((define (Start) (img (node (Choice2)) (node (Choice2))))
  ((define (Choice1) (choose 1 2))
    (define (Choice2) (choose (p (Choice1)) b)))))

(define output-tree (run-chart-parse test-scfg '(img (node b) (node (p 2)))))

(pretty-print output-tree)
(print "probability: ~s" (parse-tree->prob output-tree))

