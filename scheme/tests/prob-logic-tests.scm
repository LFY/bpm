(import (rnrs)
        (prob-logic)
        (printing)
        (program)
        (dearguments)
        (abstract)
        (background-predicates)

        (_srfi :1))

(define exs1 (list
               (list 1 2 3 4)
               (list 2 3 4 5)
               (list 3 4 5 6)
               (list 4 5 6 7)
               (list 7 8 9 10)))

(define row-soft-facts (map get-all-soft-facts exs1))

(pretty-print row-soft-facts)

(define learned-facts (find-common-soft-facts row-soft-facts))

(pretty-print (soft-facts-to-predicate learned-facts))


(define f1 (make-named-abstraction 'F '(+ X Y) '(X Y)))
(define prog1 (make-program (list f1) '(+ (F 4 4) (F 2 2))))

(define prog1-noisy1 (make-program (list f1) '(+ 
                                               (F 3.8 3.9) 
                                               (F 4.5 4.2)
                                               (F 1.1 0.9)
                                               (F 0.2 0.2)
                                               (F 3.3 3.4)
                                               (F 1.1 1.2))))

(define prog1-noisy2 (make-program (list f1) '(+ 
                                               (F 3.8 3.9) 
                                               (F 4.2 4.5)
                                               (F 0.9 1.1)
                                               (F 0.2 0.5)
                                               (F 3.3 3.4)
                                               (F 1.1 1.2))))

(pretty-print (learn-soft-predicates prog1 f1))
(pretty-print (learn-soft-predicates prog1-noisy1 f1))
(pretty-print (learn-soft-predicates prog1-noisy2 f1)) ; might also want soft-gt here...

; Deterministic examples

(define prog1-f1-facts (learn-facts prog1 f1))

(pretty-print prog1-f1-facts)

(pretty-print (derive-equalities prog1-f1-facts))

(define f2 (make-named-abstraction 'F '(list X1 X2 X3 X4) '(X1 X2 X3 X4)))
(define prog2 (make-program (list f2) '(list (F 0 0 1 1)
                                             (F 1 1 2 2)
                                             (F 3 3 4 4)
                                             (F 4 4 5 5))))
(define prog2-f2-facts (learn-facts prog2 f2))
(define prog2-f2-eq-classes (derive-equalities prog2-f2-facts))
(define prog2-f2-simplified-facts (simplify-facts prog2-f2-eq-classes prog2-f2-facts))
(define prog2-f2-simplified-facts (simplify-facts prog2-f2-eq-classes prog2-f2-facts))
(define prog2-f2-addition-eqs (derive-addition-equations prog2-f2-simplified-facts))

(print "starting facts:")
(pretty-print prog2-f2-facts)

(print "equivalence classes:")
(pretty-print prog2-f2-eq-classes)

(print "simplified facts:")
(pretty-print prog2-f2-simplified-facts)

(print "addition equations (WIP):")
(pretty-print prog2-f2-addition-eqs)

(print "feature induction tests:")

(define data1 (list
               (list 1 2 3 4)
               (list 2 3 4 5)
               (list 4 5 6 7)))

(define initial-hyp '())

(print "concept: x_i > x_{i - 1}, x_i, x_{i - 1} are roughly off by 1")

(print "stopping condition: threshold based:")
(feature-induction -10.0 simple-soft-predicates data1 initial-hyp)

(print "going for 50 iterations:")
(feature-induction-n-iter 50 simple-soft-predicates data1 initial-hyp)

;(define data1-hyp (induce-one-step simple-soft-predicates data1 initial-hyp))
;(print-hypothesis-score data1-hyp)

