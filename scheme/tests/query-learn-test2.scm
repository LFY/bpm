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
    (print "new test")
    (print "before transform:")
    (pretty-print-program prog)
    (print "after transform:")
    (pretty-print-program (query-transform-substitute-equations prog (program->lookup-abstraction prog 'F1)))))

(define (mk-prog-body p) (list 'lambda '() p))

(define test1 (make-program 
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

(define test2 (make-program 
                (list 
                  (make-named-abstraction 'F1 '(node (data (A0 V0) 
                                                           (A1 V1) 
                                                           (A2 V2)
                                                           (A3 V3)
                                                           )) '(V0 V1 V2 V3))) 
                (mk-prog-body '(node (data) 
                                     (F1 1 2 3 4) 
                                     (F1 2 3 4 5)
                                     (F1 3 4 5 6)
                                     (F1 6 7 8 9)
                                     (F1 10 11 12 13)))))

(define test3 (make-program 
                (list 
                  (make-named-abstraction 'F1 '(node (data (A0 V0) 
                                                           (A1 V1) 
                                                           (A2 V2)
                                                           (A3 V3)
                                                           )) '(V0 V1 V2 V3))) 
                (mk-prog-body '(node (data) 
                                     (F1 0 2 4 6) 
                                     (F1 3 5 7 9)
                                     (F1 5 7 9 11)
                                     (F1 10 12 14 16)))))
(define test4 (make-program 
                (list 
                  (make-named-abstraction 'F1 '(node (data (A0 V0) 
                                                           (A1 V1) 
                                                           (A2 V2)
                                                           (A3 V3)
                                                           )) '(V0 V1 V2 V3))) 
                (mk-prog-body '(node (data) 
                                     (F1 1 2 2 1) 
                                     (F1 2 4 4 2)
                                     (F1 3 6 6 3)
                                     (F1 6 12 12 6)))))

(define test5 (make-program 
                (list 
                  (make-named-abstraction 'F1 '(node (data (A0 V0) 
                                                           (A1 V1) 
                                                           (A2 V2)
                                                           )) '(V0 V1 V2))) 
                (mk-prog-body '(node (data) 
                                     (F1 1 2 3) 
                                     (F1 2 4 5)
                                     (F1 3 6 7)
                                     (F1 6 12 13)))))


(define test6 (make-program 
                (list 
                  (make-named-abstraction 'F1 '(node (data (A0 V0) 
                                                           (A1 V1) 
                                                           (A2 V2)
                                                           (A3 V3)
                                                           (A4 V4)
                                                           )) '(V0 V1 V2 V3 V4))) 
                (mk-prog-body '(node (data) 
                                     (F1 1 2 3 4 5) 
                                     (F1 3 4 5 6 7)
                                     (F1 4 5 6 7 8)
                                     ))))

(test-one test1)
(test-one test2)
(test-one test3) ;; want: repeated compositions of ( .. + 2)
(test-one test4) 
(test-one test5) ;; want: 2 * x + 1
(test-one test6) ;; want: 2 * x + 1


