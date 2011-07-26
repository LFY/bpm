(import (rnrs)
        (prob-logic)
        (printing)
        (program)
        (dearguments)
        (abstract)
        (background-predicates)

        (_srfi :1))


(define scores '())

(define (run-multiple-data data-names)
  (define (build-table next-col acc)
    (append acc next-col))
  (define (run-one-data data-name)
    (let* ([data (first data-name)]
           [name (second data-name)]
           [hyp-scores (feature-induction-full simple-soft-predicates data '() '())]
           [first-col (first (second hyp-scores))]
           [named-first-col (cons name first-col)])
      (list named-first-col)))
  (fold build-table '() (map run-one-data data-names)))

(define data-var-0.100000 (list (list 1.19058938956 2.14590494433 3.29163508748 4.2049309681 )
                                (list 9.06354340494 9.93166670106 11.0117538958 12.1121609378 )
                                (list 0.825637586075 1.75497611466 2.81192626973 3.9430529626 )
                                (list 10.0551810444 10.9526581854 12.0757939095 13.0498385084 )
                                (list 4.02510346294 5.06762082551 6.0696397375 6.91540377048 )
                                (list 1.11822327879 2.08353188739 3.09704429363 4.08242621125 )
                                (list 4.02346605502 4.93578504461 5.63627388816 6.73108916111 )
                                (list 9.05058444031 10.0243190367 11.0355671393 12.2279101192 )
                                (list 9.03534946571 9.95691617438 10.9553003336 11.769630452 )
                                (list 10.0007554754 11.0956584786 12.0015537353 12.9856606221 ) ))

(define results (run-multiple-data (list 
                                     (list data-var-0.100000 'data-var-0.100000))))
(print (format-csv results))
