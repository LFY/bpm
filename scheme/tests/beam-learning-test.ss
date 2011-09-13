(import (rnrs)
        (_srfi :1)
        (chart-parsing)
        (named-search-trees)

        (program)
        (abstract)
        (dearguments)
        (inverse-inline)
        (sym)

        (program-likelihood)

        (util)

        (beam-learning)

        (printing))

(define (incorporate-data xs)
  (list 'lambda '() (cons 'choose xs)))
(define (learn-model2 data beam-size depth)

  (define (program->transforms prog)
    (begin
      (cons prog (append (compressions prog) 
                         (uniform-choice-compressions prog)
                         ;; (uniform-choose-dearguments prog)
                         ;; (recursive-choose-dearguments prog)
                         ;; (arith-dearguments prog)
                         ;; (arith-recursion-dearguments prog)
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
    '(node (data (name NULL))
           (node
             (data (name HTML) (left 0) (top 0) (width 962)
                   (height 812))
             (node
               (data (name BODY) (left 0) (top 0) (width 962)
                     (height 716))
               (node
                 (data (name DIV) (left 0) (top 0) (width 910)
                       (height 716))
                 (node
                   (data (name DIV) (left 50) (top 67) (width 250)
                         (height 377))
                   (node
                     (data (name A) (left 50) (top 67) (width 244)
                           (height 118)))
                   (node
                     (data (name UL) (left 50) (top 238) (width 250)
                           (height 115))
                     (node
                       (data (name LI) (left 50) (top 238)
                             (width 250) (height 23)))
                     (node
                       (data (name LI) (left 50) (top 261)
                             (width 250) (height 23)))
                     (node
                       (data (name LI) (left 50) (top 284)
                             (width 250) (height 23)))
                     (node
                       (data (name LI) (left 50) (top 307)
                             (width 250) (height 23)))
                     (node
                       (data (name LI) (left 50) (top 330)
                             (width 250) (height 23)))))
                 (node
                   (data (name DIV) (left 325) (top 238) (width 575)
                         (height 345))
                   (node
                     (data (name H1) (left 325) (top 238) (width 575)
                           (height 23)))
                   (node
                     (data (name P) (left 325) (top 266) (width 575)
                           (height 72)))
                   (node
                     (data (name H2) (left 325) (top 356) (width 575)
                           (height 23)))
                   (node
                     (data (name P) (left 325) (top 384) (width 575)
                           (height 54)))
                   (node
                     (data (name P) (left 325) (top 456) (width 575)
                           (height 54))))
                 (node
                   (data (name DIV) (left 50) (top 583) (width 850)
                         (height 122))
                   (node
                     (data (name P) (left 325) (top 594) (width 575)
                           (height 48))))))))))

(define test-data2 (list '(node (node (node (node (node (node (node (node (node (node (node (node 1))))))))))))
                         '(node (node (node (node (node (node (node (node (node (node (node (node 1))))))))))))))

(define test-data3 (list '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)
                         '(node 1)))

(print (length (possible-abstractions (condense-program (incorporate-data test-data3)))))
(time (pretty-print (learn-model2 test-data3 1 15)))

(print (length (possible-abstractions (condense-program (incorporate-data test-data2)))))
(time (pretty-print (learn-model2 test-data2 1 15)))

;; (learn-model2 test-data 1 15)
