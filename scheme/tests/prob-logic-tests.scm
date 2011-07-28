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
(feature-induction-threshold -10.0 simple-soft-predicates data1 initial-hyp '())

(print "going for 50 iterations:")
(feature-induction-n-iter 50 simple-soft-predicates data1 initial-hyp '())

(define data1-noisy (list (list 4.01952871479 4.93702023585 5.95398612597 7.07566661085 )
                    (list 9.12955621101 10.2688397464 11.3151882432 12.404088121 )
                    (list 4.04473396656 5.08696659048 6.01901585978 6.88851430631 )
                    (list 1.17182461124 2.22584402121 3.11922875183 4.40644694063 )
                    (list 9.89317668847 10.8069455774 11.8307372734 12.9816011493 )
                    (list 9.14639820991 10.0845469415 11.0104746622 12.2587408509 )
                    (list 5.16040678965 6.18747414978 7.21862756883 8.16709942331 )
                    (list 4.8346728059 5.76978465949 6.66444529594 7.67925448369 )
                    (list 10.0344349538 10.8992241587 11.9741302366 13.0222910233 )
                    (list 10.033509527 11.1050602761 12.1898333101 13.0605330368 ) ))

(print "same concept, but noisy data. (generated from ground truth model)")

(print "stopping condition: threshold based:")
(feature-induction-threshold -10.0 simple-soft-predicates data1-noisy initial-hyp '())

(print "going for 50 iterations:")
(feature-induction-n-iter 50 simple-soft-predicates data1-noisy initial-hyp '())

;(define data1-hyp (induce-one-step simple-soft-predicates data1 initial-hyp))
;(print-hypothesis-score data1-hyp)

