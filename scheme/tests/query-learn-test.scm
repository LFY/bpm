(import (rnrs)
        (query-learn)
        (sym)
        (printing)
        (program)
        (dearguments)
        (abstract)
        (_srfi :1))

(define f1 (make-named-abstraction 'F '(+ X Y) '(X Y)))
(define before-query (make-program (list f1) '(+ (F 4 4) (F 2 2))))

(println "basic query-learn test:")
(pretty-print-program before-query)
(pretty-print-program (query-transform before-query f1))

(define prog2 
  (make-program
    (list f1)
    '(+ (F 1 2) (F 2 3) (F 3 4))))

(pretty-print-program prog2)
(pretty-print-program (query-transform prog2 f1))

(define f2 (make-named-abstraction 'G '(list X Y Z) '(X Y Z)))

(define prog3
  (make-program
    (list f2)
    '(+ (G 1 1 1) (G 2 2 2) (G 3 3 3))))

(pretty-print-program prog3)
(pretty-print-program (query-transform prog3 f2))

(define prog4
  (make-program
    (list f2)
    '(+ (G 1 2 3) (G 2 3 4) (G 3 4 5))))

(pretty-print-program prog4)
(pretty-print-program (query-transform prog4 f2))

(define prog5
  (make-program
    (list f2)
    '(+ (G 0 0 0) (G 1 0 -1) (G 2 3 -2))))

(pretty-print-program prog5)
(pretty-print-program (query-transform prog5 f2))

(define prog6
  (make-program
    '()
    '(+ (1 1))))

(println "try to get query-transforms to break")
(pretty-print-program prog6)
(println "for prog5:")
(pretty-print (query-transforms prog5 'nofilter))
(println "for prog6:")
(pretty-print (query-transforms prog6 'nofilter))

; TODO: make some test cases that use the (let () (uniform-choice ...))

(define (mk-initial-prog data)
  (make-program '() (list 'lambda '() `(uniform-draw (list ,@data)))))

(define data1
  (map (lambda (i) `(list ,i ,i ,i)) (iota 10)))

(define prog7 (mk-initial-prog data1))

(println "testing prog size calculation:")
(pretty-print-program prog7)
(define prog7-compress (first (compressions prog7 'nofilter)))
(pretty-print-program prog7-compress)
(define prog7-compress-query (first (query-transforms prog7-compress 'nofilter)))
(pretty-print-program prog7-compress-query)


;(define test-facts
;  (map (lambda (p) (list p '(1 2 3))) background-predicates))
;
;(println "our background predicates:")
;(println test-facts)
;
;(println "arg matrix test:")
;(println (arg-matrix before-query f1))
;
;(println "learn-predicate test:")
;(println (learn-predicates before-query f1))
;
;(println "other tests")
;(define t (list equal? '((0) (1))))
;(define s (list equal? '((0) (1))))
;(define v (list even? '((0) (1))))
;(define l (list v v v))
;(println (equal? t s))
;(println (contains? t l))
;(println (contains? v l))
;
