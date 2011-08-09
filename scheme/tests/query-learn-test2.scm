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

(define (mk-prog-body p) (list 'lambda '() p))

(define f1 (make-named-abstraction 'F1 '(node (data (A0 V0) 
                                                    (A1 V1) 
                                                    (A2 V2)
                                                    (A3 V3)
                                                    (A5 V5))) '(V0 V1 V2 V3)))

(define before-query (make-program (list f1) 
                                   (mk-prog-body '(node (data) 
                                                        (F1 1 -1 2 -2) 
                                                        (F1 2 -2 3 -3)
                                                        (F1 4 -4 5 -5)
                                                        (F1 7 -7 8 -8)
                                                        (F1 9 -9 10 -10)))
                                   ))

(println "basic query-learn test:")
(pretty-print-program before-query)
(pretty-print-program (query-transform before-query f1))

(println "with substitutions from predicates:")

(print "learn-predicates:")
(define pred (learn-predicates-keep-vars before-query f1))
(define facts (pred->facts pred))
(pretty-print pred)
(pretty-print facts)

(print "generate-substitutions:")
(define subbed (generate-substitutions pred))
(pretty-print subbed)

(print "the full function:")
(define after-query (query-transform-substitute-equations before-query f1))

(print "before substitution:")
(pretty-print-program before-query)

(print "after substitution:")
(pretty-print-program after-query)
