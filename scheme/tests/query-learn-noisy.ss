(import (rnrs)
        (query-learn)
        (sym)
        (printing)
        (program)
        (dearguments)
        (abstract)
        (prob-logic)
        (graph)
        (util)
        (_srfi :1))

(define (test-one prog)
  (begin
    (print "new test")
    (print "before transform:")
    (pretty-print-program prog)
    (print "after transform (ordinary substitution):")
    (pretty-print-program (query-transform-substitute-equations-noisy prog (program->lookup-abstraction prog 'F1)))

    (print "after transform (recursion):")
    (print (query-recursion-transforms-noisy prog 'nofilter))
    
    ))

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
                                            (F1 90 -90 91 -91)))
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

(define test7 (make-program 
                (list 
                  (make-named-abstraction 'F1 '(node (data (A0 V0) 
                                                           (A1 V1) 
                                                           )) '(V0 V1))) 
                (mk-prog-body '(node (data) 
                                     (F1 1 10) 
                                     (F1 30 300)
                                     (F1 4 40)
                                     ))))
(define test8 (make-program
                (list (make-named-abstraction 'F1 '(node (data (A0 V1)) (node (data (A0 V2)) V3)) '(V1 V2 V3)))
                (mk-prog-body
                  '(node (data)
                         (F1 1 2
                             (F1 3 4
                                 (F1 5 6
                                     (F1 7 8
                                         (node (data (A0 9)) (node (data (A0 10))))))))
                         (F1 1 2
                             (F1 3 4 (F1 5 6 (F1 7 8 (node (data (A0 9)))))))
                         (F1 1 2
                             (F1 3 4
                                 (F1 5 6 (node (data (A0 7)) (node (data (A0 8)))))))
                         (F1 1 2 (F1 3 4 (F1 5 6 (node (data (A0 7))))))
                         (F1 1 2
                             (F1 3 4 (node (data (A0 5)) (node (data (A0 6))))))
                         (F1 1 2 (F1 3 4 (node (data (A0 5)))))
                         (F1 1 2 (node (data (A0 3)) (node (data (A0 4)))))
                         (F1 1 2 (node (data (A0 3))))
                         (node (data (A0 1)) (node (data (A0 2))))
                         (node (data (A0 1))))
                  )))

;; (test-one test1)
;; (test-one test2)
;; (test-one test3) 
;; (test-one test4) 
;; (test-one test5) 
;; (test-one test6) 
;; (test-one test7) 
(test-one test8) 
;; (pretty-print (map (curry apply zip) (arg-matrix-by-chain test8 (program->lookup-abstraction test8 'F1))))



