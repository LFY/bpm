(import (dearguments)
        (query-learn)
        (prob-logic)
        (program)
        (util)
        (sym)
        (printing)
        (_srfi :1))

(define (mk-prog-body p) (list 'lambda '() p))

(define test1 (make-program 
                (list 
                  (make-named-abstraction 'F1 '(node (data (A0 V0)) V1) '(V0 V1))) 
                (mk-prog-body '(node (data) 
                                     (F1 1 (F1 2 (F1 3 (F1 4 (node (data (A0 5)))))))
                                     

                                     ))
                ))

(define test2 (make-program 
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
(define test3 (make-program 
                (list 
                  (make-named-abstraction 'F1 '(node (data (A0 V0) (A1 V1)) V2) '(V0 V1 V2))) 
                (mk-prog-body '(node (data) 
                                     (F1 1 3 (F1 2 4 (F1 3 5 (F1 4 6 (node (data (A0 5)))))))
                                     

                                     ))
                ))

(define test4 (make-program 
                (list 
                  (make-named-abstraction 'F1 '(node (data (A0 V0) (A1 V1))) '(V0 V1))) 
                (mk-prog-body '(node (data) 
                                     (F1 1 3)
                                     

                                     ))
                ))

(pretty-print test1)
(pretty-print (query-recursion-transforms test1 'nofilter))
(print "all-subexprs:")
(for-each print (all-subexprs (program->body test2)))
(print "shallow-find-all")
(pretty-print (shallow-find-all (lambda (x) (eq? 'F1 x)) (program->body test2)))
(print "arg matrix by chain")
(pretty-print (arg-matrix-by-chain test2 (program->lookup-abstraction test2 'F1)))

(print "multiple-chain-test:")
(pretty-print (query-recursion-transforms test2 'nofilter))

(print "interaction w/ other rule:")
(print "rows then cols:")
(pretty-print (query-recursion-transforms (first (query-transforms test3 'nofilter)) 'nofilter))
(print "cols then rows:")
(pretty-print (query-transforms (first (query-recursion-transforms test3 'nofilter)) 'nofilter))

(print "problem case----------------------------------------:")
(define recursion-transformed-prog (first (query-recursion-transforms test3 'nofilter)))
(define substituted-prog (first (uniform-draw-dearguments recursion-transformed-prog 'nofilter)))
(define pred-substituted-prog (first (query-transforms recursion-transformed-prog 'nofilter)))

(print "recursion-transformed-prog:")
(pretty-print recursion-transformed-prog)

(print "plain substitution dearguments:")
(pretty-print substituted-prog)

(print "predicate-based substitutino dearguments:")
(pretty-print pred-substituted-prog)

(print "debug------------------------------------------------:")
(pretty-print (query-transforms test4 'nofilter))
;; (pretty-print (arg-matrix test1 (program->lookup-abstraction test1 'F1)))
;; (pretty-print (get-column-predicates test1 (program->lookup-abstraction test1 'F1)))

