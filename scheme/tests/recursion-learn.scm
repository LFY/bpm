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

(define test1-noisy (make-program 
                (list 
                  (make-named-abstraction 'F1 '(node (data (A0 V0)) V1) '(V0 V1))) 
                (mk-prog-body '(node (data) 
                                     (F1 0.9 (F1 1.1 (F1 2 (F1 3.3 (node (data (A0 5)))))))
                                     ))
                ))

(define test1-dearg (noisy-arith-recursion-dearguments test1-noisy 'nofilter))

(pretty-print test1-dearg)
