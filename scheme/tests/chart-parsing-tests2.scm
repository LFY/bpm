(import (rnrs)
        (_srfi :1)
        (printing)
        (util)
        (chart-parsing)
        (program)
        (named-search-trees)
        (program-likelihood))

(define test-scfg 
  '(
    (define (Start) (img (node (Choice2)) (node (Choice2))))

  (
   (define (Choice2) (choose (p (Choice1)) 
                             HTML))
   (define (Choice1) (choose 1 2))
   )))

(define output-tree 
  (run-chart-parse test-scfg 
                   '(img (node HTML) 
                         (node (p 2)))))

(pretty-print output-tree)
(print "probability: ~s" (parse-tree->prob output-tree))

(define test-scfg2
  '(
    (define (Start) (choose (er (my5 (Start)))
                            (er (my5 (er)))
                            (er (my5 (er (my5 (er)))))
                            ))

  (
   )))

(define output-tree 
  (run-chart-parse test-scfg2
                   '(er (my5 (er (my5 (er (my5 (er)))))))))

(pretty-print output-tree)
(print "probability: ~s" (parse-tree->prob output-tree))

'((tree sym_Start 0 3
        ((tree sym_Start 0 3 ((tree sym_Start 1 3)))
         (tree sym_Start 2 3))))

