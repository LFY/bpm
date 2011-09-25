(import (rnrs)
        (query-learn)
        (sym)
        (printing)
        (program)
        (dearguments)
        (abstract)
        (prob-logic)
        (graph)
        (_srfi :1))


(define (test-one prog)
  (begin
    (print "program: ")
    (pretty-print-program prog)
    (print "argument matrices of F1 (grouped by chain):")
    (pretty-print (map (lambda (m) (apply zip m)) (arg-matrix-by-chain prog (program->lookup-abstraction prog 'F1))))
    (print "fully transformed:")
    (let* ([transformed (query-recursion-transforms prog 'nofilter)])
      (if (null? transformed)
        (pretty-print-program (first (query-transforms prog 'nofilter)))
        (pretty-print-program (first (query-transforms (first transformed) 'nofilter)))))
    )
  )

(define (mk-prog-body p) (list 'lambda '() p))

(define nonrec (make-program 
                       (list 
                         (make-named-abstraction 'F1 '(node (data (A0 V0) 
                                                                  (A1 V1) 
                                                                  (A2 V2)
                                                                  (A3 V3)
                                                                  )) '(V0 V1 V2 V3))) 
                       (mk-prog-body '(node (data) 
                                            (F1 1 -1 2 -2) 
                                            (F1 2 -2 3 -3)
                                            (F1 4 -4 5 -5)
                                            (F1 7 -7 8 -8)
                                            (F1 9 -9 10 -10)))
                       ))

(define rec1 (make-program 
                (list 
                  (make-named-abstraction 'F1 '(node (data (A0 V0)) V1) '(V0 V1))) 
                (mk-prog-body '(node (data) 
                                     (F1 1 (F1 2 (F1 3 (F1 4 (node (data (A0 5)))))))
                                     

                                     ))
                ))

(define rec2 (make-program 
                (list 
                  (make-named-abstraction 'F1 '(node (data (A0 V0)) V1) '(V0 V1))) 
                (mk-prog-body '(node (data) 
                                     (F1 2 (F1 3 (node (data (A0 4)))))
                                     (node (data)
                                           (F1 1 (F1 2 (F1 3 (F1 4 (node (data (A0 5)))))))
                                           (F1 3 (F1 4 (F1 5 (F1 6 (node (data (A0 7))))))))
                                     (F1 5 (F1 6 (node (data (A0 6)))))


                                     ))
                ))

(define rec3 (make-program 
                (list 
                  (make-named-abstraction 'F1 '(node (data (A0 V0) (A1 V1)) V2) '(V0 V1 V2))) 
                (mk-prog-body '(node (data) 
                                     (F1 1 3 (F1 2 4 (F1 3 5 (F1 4 6 (node (data (A0 5)))))))
                                     

                                     ))
                ))

(test-one nonrec)
(test-one rec1)
(test-one rec2)
(test-one rec3)
