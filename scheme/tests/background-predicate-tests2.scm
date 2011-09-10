(import (background-predicates)
        (printing))

(print "unification tests")

(define pa1 (list soft-eq? (list 1 'H)))
(define pa2 (list soft-offby1? (list 'H 2)))
(define pa3 (list soft-neg? (list -1 'H)))

(print (unify (list pa1)))
(print (unify (list pa2)))
(print (unify (list pa3)))
(print (unify (list pa1 pa2 pa3)))

(print "parameterized predicate tests")

(print (apply-param-pred (mk-param-pred offbyN? 5) 0 5))
(print (apply-param-pred (mk-param-pred offbyN? 1) 0 5))
(print (apply-param-pred (mk-param-pred offbyN? 2) 0 2))
(print (param-pred? (mk-param-pred offbyN? 3)))

(print (equal? (mk-param-pred offbyN? 3)
               (mk-param-pred offbyN? 3)))

(print (equal? (mk-param-pred offbyN? 4)
               (mk-param-pred offbyN? 3)))

(print (derive-param-pred offbyN? 1 2))
(print (pred->name (derive-param-pred offbyN? 1 2)))
(print (pred->param (derive-param-pred offbyN? 1 2)))
(print (pred->arity (derive-param-pred offbyN? 1 2)))
(print (commutative? (derive-param-pred offbyN? 1 2)))
(print (can-apply? (derive-param-pred offbyN? 1 2) '(1 2)))

(print (arithmetic-rule (list '(X Y) (list offby1? >))))
(print (arithmetic-rule2 (list '(X Y) (list offby3? (derive-param-pred offbyN? 2 5) >))))
(print (arithmetic-rule (list '(X Y) (list (derive-param-pred offbyN? 2 5) <))))
