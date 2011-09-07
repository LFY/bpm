(library (program-likelihood)
         (export log-prob-sum
                 parse-tree->prob
                 parse-tree->log-prob
                 
                 data-program->log-likelihood
                 data-program->log-posterior)
         (import (rnrs)
                 (_srfi :1)
                 (chart-parsing)
                 (util)
                 (sym)
                 (printing)
                 (program))

         (define (parse-tree->prob tree)
           (cond [(eq? 'tree (car tree)) (let* ([subtrees (cddddr tree)]
                                                [my-prob (/ 1 (cadddr tree))])
                                           (* my-prob 
                                              (apply * (map parse-tree->prob subtrees))))]
                 [(list? (car tree)) (apply + (map parse-tree->prob tree))]
                 [else 1]))

         (define (log-prob-sum . xs)
           (define (bin-log-prob x y)
             (+ y (log (+ 1 (exp (- x y))))))
           (fold bin-log-prob (car xs) (cdr xs)))

         (define (parse-tree->log-prob tree)
           (cond [(eq? 'tree (car tree)) (let* ([subtrees (cddddr tree)]
                                                [my-prob (log (/ 1 (cadddr tree)))])
                                           (+ my-prob 
                                              (apply + (map parse-tree->log-prob subtrees))))]
                 [(list? (car tree)) (apply log-prob-sum (map parse-tree->prob tree))]
                 [else 1]))

         (define (data-program->log-likelihood data prog)
           (parse-tree->log-prob (run-chart-parse (program->scfg prog) data)))

         (define (data-program->likelihood data prog)
           (parse-tree->prob (run-chart-parse (program->scfg prog) data)))

         (define (program->prior prog)
           (- (program-size prog)))

         (define (data-program->log-posterior data prog)
           (+ (data-program->log-likelihood data prog) (program->prior prog)))

           )

