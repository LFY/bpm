(import (rnrs)
        (_srfi :1)
        (printing)
        (util)
        (chart-parsing)
        (program)
        (named-search-trees)
        (program-likelihood))

(define (mk-prog-body p) (list 'lambda '() p))
(define test1 (make-program 
                       (list 
                         (make-named-abstraction 'f1 '(node (data (a0 v0) 
                                                                  (a1 v1) 
                                                                  (a2 v2)
                                                                  (a3 v3)
                                                                  )
                                                            (choose v0 v1)
                                                            
                                                            ) '(v0 v1 v2 v3))) 
                       (mk-prog-body '(node (data (a0 111)) 
                                            (f1 1 -1 2 -2) 
                                            (f1 2 -2 3 -3)
                                            (f1 4 -4 5 -5)
                                            (f1 7 -7 8 -8)
                                            (f1 90 -90 91 -91)))
                       ))

(define test2 (make-program '()
                            (mk-prog-body '(node (data) (p 0) (p 1)))))

(define (parse-tree->prob tree)
  (cond [(eq? 'tree (car tree)) (let* ([subtrees (cddddr tree)]
                                       [my-prob (/ 1 (cadddr tree))])
                                  (* my-prob 
                                     (apply * (map parse-tree->prob subtrees))))]
        [(list? (car tree)) (apply + (map parse-tree->prob tree))]
        [else 1]))

(define test-scfg '((define (start) (img (node (choice2)) (node (choice2))))
  ((define (choice1) (choose 1 2))
    (define (choice2) (choose (p (choice1)) HTML)))))

(define output-tree (run-chart-parse test-scfg '(img (node HTML) (node (p 2)))))

;; (pretty-print (program->scfg test2))
(pretty-print output-tree)
(print "probability: ~s" (parse-dag->log-prob output-tree))


(define test3 (make-program (list (make-named-abstraction 'f1 '(node v0 (+ 1 v0)) '(v0)))
                            (mk-prog-body '(node (data)
                                                 (f1 1)
                                                 (f1 2)
                                                 (f1 3)))))
(pretty-print (program->scfg test3))

(define-nondet (test4 x)
               (let* ([y (nondet-choice 1 2)])
                 `(node ,x ,y ,y)))

(define-nondet (test4-p) (test4 1))

(pretty-print (nondet-program->named-search-tree test4-p)) ;; sometime later: add 'let' (for choices) to named-search-trees




