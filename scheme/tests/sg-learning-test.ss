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
                                  ;; (noisy-compressions prog)
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
    '(asm (data)
          (translate (data (name "tr1") (offset '(0 0)))
                     (model
                       (data (filename "triangle.svg") (pos '(0 0))
                             (bbox_min '(0 0)) (bbox_max '(10 10)) (scale 1.0))))
          (translate (data (name "tr2") (offset '(100 0)))
                     (model
                       (data (filename "triangle.svg") (pos '(0 0))
                             (bbox_min '(0 0)) (bbox_max '(10 10)) (scale 1.0))))
          (translate (data (name "tr3") (offset '(100 100)))
                     (model
                       (data (filename "triangle.svg") (pos '(0 0))
                             (bbox_min '(0 0)) (bbox_max '(10 10)) (scale 1.0))))
          (translate (data (name "tr4") (offset '(0 100)))
                     (model
                       (data (filename "triangle.svg") (pos '(0 0))
                             (bbox_min '(0 0)) (bbox_max '(10 10)) (scale 1.0))))
          (figure (data (name "box1"))))))

(pretty-print (learn-model2 test-data 1 15))
