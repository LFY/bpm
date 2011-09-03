(import (rnrs)
        (query-learn)
        (sym)
        (printing)
        (program)
        (dearguments)
        (abstract)
        (_srfi :1))

(print "void? ~s" arith-deargument-transform)
(define f1 (make-named-abstraction 'F '(+ X Y) '(X Y)))
(define before-query (make-program (list f1) '(+ (F 4 4) (F 2 2))))

(println "basic query-learn test:")
(pretty-print-program before-query)
(pretty-print-program (first (arith-recursion-dearguments before-query f1)))

(define prog2 
  (make-program
    (list f1)
    '(+ (F 1 2) (F 2 3) (F 3 4))))

(pretty-print-program prog2)
(pretty-print-program (first (arith-dearguments prog2 f1)))

(define f2 (make-named-abstraction 'G '(list X Y Z) '(X Y Z)))

(define prog3
  (make-program
    (list f2)
    '(+ (G 1 1 1) (G 2 2 2) (G 3 3 3))))

(pretty-print-program prog3)
(pretty-print-program (first (arith-dearguments prog3 f2)))

(define prog4
  (make-program
    (list f2)
    '(+ (G 1 2 3) (G 2 3 4) (G 3 4 5))))

(pretty-print-program prog4)
(pretty-print-program (first (arith-dearguments prog4 f2)))

(define prog5
  (make-program
    (list f2)
    '(+ (G 0 0 0) (G 1 0 -1) (G 2 3 -2))))

(pretty-print-program prog5)
(pretty-print-program (first (arith-dearguments prog5 f2)))

