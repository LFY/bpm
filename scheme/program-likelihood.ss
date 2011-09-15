(library (program-likelihood)
         (export log-prob-sum
                 parse-tree->prob
                 parse-tree->log-prob
                 
                 data-program->log-likelihood
                 data-program->log-posterior

                 data-program->posterior
                 
                 no-choices?
                 batch-data-program->posterior)
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
           (cond [(null? tree) -inf.0]
                 [(eq? 'tree (car tree)) (let* ([subtrees (cddddr tree)]
                                                [my-prob (log (/ 1 (cadddr tree)))])
                                           (+ my-prob 
                                              (apply + (map parse-tree->log-prob subtrees))))]
                 [(list? (car tree)) (apply log-prob-sum (map parse-tree->prob tree))]
                 [else 1]))

         (define (no-choices? prog)
           (let* (;; [condensed-program (condense-program prog)]
                  [choices (deep-find-all (lambda (x) (cond [(list? x) (cond [(eq? 'choose (car x)) (> (length (cdr x)) 1)]
                                                                             [else #f])]
                                                            [else #f])) prog)])
             (null? choices)))

         (define (data-program->log-likelihood data prog)
           (if (no-choices? prog) 0.0
             (parse-tree->log-prob (run-chart-parse (program->scfg prog) data))))

         (define (data-program->likelihood data prog)
           (if (no-choices? prog) 1.0
             (parse-tree->prob (run-chart-parse (program->scfg prog) data))))

         (define (program->prior prog)
           (- (program-size prog)))

         (define (data-program->log-posterior data prog)
           (+ (data-program->log-likelihood data prog) (program->prior prog)))

         (define (data-program->posterior data prog)
           (* (data-program->likelihood data prog) (exp (program->prior prog))))

         (define (batch-data-program->posterior data progs)
           (define (iterator charts 
                             programs 
                             scores)
             (cond [(null? programs) (reverse scores)]
                   [(no-choices? (car programs)) (iterator charts 
                                                           (cdr programs) 
                                                           (cons (+ 0.0 (program->prior (car programs))) scores))]
                   [else (iterator (cdr charts) 
                                   (cdr programs) 
                                   (cons (+ (apply + (map parse-tree->log-prob (car charts)))
                                            (program->prior (car programs))) scores))]))

           (let* ([progs-with-choices (filter (lambda (p) (not (no-choices? p))) progs)]
                  [all-charts (if (null? progs-with-choices) '() (batch-run-chart-parse (map program->scfg progs-with-choices) data))]
                  [scores (iterator all-charts progs '())])
             (begin ;; (print "batch scores: ~s" scores)
                    scores)))

           )

