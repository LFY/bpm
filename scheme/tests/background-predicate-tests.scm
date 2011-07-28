(import (background-predicates)
        (printing))

; (print "soft predicate tests")
; (print "soft-eq tests")
; (print (soft-eq? 1 2))
; (print (soft-eq? 1 1.5))
; (print (soft-eq? 1 1.2))
; (print (soft-eq? 1 1.1))
; (print (soft-eq? 1 1.05))
; 
; (print "soft-greater tests")
; (print (soft-greater? 2 1))
; (print (soft-greater? 1 2))
; (print (soft-greater? 3 5))
; (print (soft-greater? 2 1))
; 
; (print "soft-offby1 tests")
; (print (soft-offby1? 1 2))
; (print (soft-offby1? 1 2.1))
; (print (soft-offby1? 2 2))
; (print (soft-offby1? 2 1))
; (print (soft-offby1? 3 1))
; 
; (print "soft-neg tests")
; 
; (print (soft-neg? 1 -1))
; 
; (print (soft-neg? 0.5 -0.5))
; (print (soft-neg? 0.4 -0.4))
; (print (soft-neg? 0.3 -0.3))
; (print (soft-neg? 0.2 -0.2))
; (print (soft-neg? 0.1 -0.1))
; 
; (print (soft-neg? 0.0 -0.5))
; (print (soft-neg? 0.0 -0.4))
; (print (soft-neg? 0.0 -0.3))
; (print (soft-neg? 0.0 -0.2))
; (print (soft-neg? 0.0 -0.1))
; 
; (print "range predicate tests")
; (print "range-eq tests")
; (print (range-eq? 1 2))
; (print (range-eq? 1 1.5))
; (print (range-eq? 1 1.2))
; (print (range-eq? 1 1.1))
; (print (range-eq? 1 1.05))
; 
; (print "range-greater tests")
; (print (range-greater? 2 1))
; (print (range-greater? 1 2))
; (print (range-greater? 3 5))
; (print (range-greater? 2 1))
; 
; (print "range-offby1 tests")
; (print (range-offby1? 1 2))
; (print (range-offby1? 1 2.1))
; (print (range-offby1? 2 2))
; (print (range-offby1? 2 1))
; (print (range-offby1? 3 1))
; 
; (print "range-neg tests")
; 
; (print (range-neg? 1 -1))
; 
; (print (range-neg? 0.5 -0.5))
; (print (range-neg? 0.4 -0.4))
; (print (range-neg? 0.3 -0.3))
; (print (range-neg? 0.2 -0.2))
; (print (range-neg? 0.1 -0.1))
; 
; (print (range-neg? 0.0 -0.5))
; (print (range-neg? 0.0 -0.4))
; (print (range-neg? 0.0 -0.3))
; (print (range-neg? 0.0 -0.2))
; (print (range-neg? 0.0 -0.1))

(define pa1 (list soft-eq? (list 1 'H)))
(define pa2 (list soft-offby1? (list 'H 2)))
(define pa3 (list soft-neg? (list -1 'H)))

(print (unify (list pa1)))
(print (unify (list pa2)))
(print (unify (list pa3)))
(print (unify (list pa1 pa2 pa3)))

