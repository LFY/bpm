(import (rnrs)
        (_srfi :1)
        (chart-parsing)
        (named-search-trees)

        (program)
        (abstract)
        (dearguments)
        (sym)

        (program-likelihood)

        (util)

        (beam-learning)

        (printing))

(define (learn-model2 data beam-size depth)
           (define (incorporate-data xs)
             (list 'lambda '() (cons 'choose xs)))

           (define (program->transforms prog)
             (begin
               (cons prog (append (compressions prog) 
                                  (uniform-choose-dearguments prog) 
                                  (arith-dearguments prog)
                                  ;; (noisy-arith-dearguments prog)
                                  ))))

           (define (program->log-posterior prog)
             (apply + (map (lambda (d) (data-program->log-posterior d prog))
                           data)))
           (define (print-stats fringe depth)
             (let ([best-prog (car fringe)])
               (begin (print "depth: ~s best program:" depth)
                      (print "posterior: ~s" (program->log-posterior best-prog))
                      (pretty-print (car fringe)))))
               

           (define (depth-stop fringe depth)
             (= 0 depth))

           (define (same-prog-stop limit)
             (define prog-store '())
             (define (add-one-prog p) (set! prog-store (cons p prog-store)))
             (define (reached-limit?)
               (let ([tail (max-take prog-store limit)])
                 (cond [(>= (length tail) limit) (= 1 (length (delete-duplicates tail)))]
                       [else #f])))

             (lambda (fringe depth)
               (begin (print-stats fringe depth)
                      (add-one-prog (car fringe))
                      (reached-limit?))))

           (let* ([initial-prog (make-program '() (incorporate-data data))]
                  [learned-program (beam-search (list initial-prog)
                                                beam-size depth
                                                program->transforms
                                                program->log-posterior
                                                (lambda (x) x)
                                                (same-prog-stop 5))])
             learned-program))


(define test-data
  (list
    '(node
       (data)
       (node
         (data (y 0) (x 0) (width 1280) (height 1024))
         (node
           (data (y 0) (x 160) (width 960) (height 41))
           (node (data (y 0) (x 163) (width 335) (height 40)))
           (node (data (y 0) (x 805) (width 315) (height 40))
             (node (data (y 0) (x 805) (width 57) (height 40)))
             (node (data (y 0) (x 862) (width 86) (height 40)))
             (node (data (y 0) (x 948) (width 85) (height 40)))
             (node (data (y 0) (x 1033) (width 74) (height 40)))))
         (node
           (data (y 41) (x 160) (width 960) (height 787))
           (node (data (y 91) (x 733) (width 397) (height 679)))
           (node
             (data (y 144) (x 160) (width 565) (height 684))
             (node (data (y 144) (x 180) (width 444) (height 80)))
             (node (data (y 244) (x 180) (width 500) (height 100)))
             (node
               (data (y 359) (x 180) (width 545) (height 469))
               (node (data (y 359) (x 180) (width 187) (height 414))
                 (node (data (y 379) (x 170) (width 167) (height 167)))
                 (node (data (y 568) (x 180) (width 143) (height 13)))
                 (node (data (y 584) (x 180) (width 167) (height 18)))
                 (node (data (y 611) (x 180) (width 167) (height 108)))
                 (node (data (y 728) (x 180) (width 167) (height 36))))
               (node (data (y 359) (x 367) (width 187) (height 414))
                 (node (data (y 379) (x 357) (width 167) (height 167)))
                 (node (data (y 568) (x 367) (width 145) (height 13)))
                 (node (data (y 584) (x 367) (width 167) (height 18)))
                 (node (data (y 611) (x 367) (width 167) (height 108)))
                 (node (data (y 728) (x 367) (width 167) (height 36))))
               (node (data (y 359) (x 554) (width 167) (height 396))
                 (node (data (y 379) (x 544) (width 167) (height 167)))
                 (node (data (y 568) (x 554) (width 121) (height 13)))
                 (node (data (y 584) (x 554) (width 167) (height 18)))
                 (node (data (y 611) (x 554) (width 167) (height 90)))
                 (node (data (y 710) (x 554) (width 167) (height 36)))))))
         (node (data (y 849) (x 180) (width 514) (height 17))
           (node (data (y 849) (x 180) (width 180) (height 17)))
           (node (data (y 849) (x 360) (width 192) (height 17)))
           (node (data (y 849) (x 552) (width 80) (height 17)))
           (node (data (y 849) (x 632) (width 62) (height 17))))))))

(pretty-print (learn-model2 test-data 1 15))
