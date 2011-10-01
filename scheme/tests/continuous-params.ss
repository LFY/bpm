(import (program-pl)
        (printing)
        (program)
        (prolog-serialize)
        (program-likelihood)
        (chart-parsing))

(define prog1 
  (make-program 
    (list 
      (make-named-abstraction 'F1
                              '(choose 0 1 2)
                              '()
                              ))
    '(lambda () (gauss (F1) (F1)))))

(define test-query (list 
                    (pl-clause (pl-relation 'go)
                             (pl-relation 'pTopLevel
                                          '4.4))))

(define prog1-relations 
  (program->pl+features prog1))

(display-pl (append prog1-relations test-query))


(define test-data 4.4)

(pretty-print (batch-run-inversion (list prog1) (list test-data) 'use-features))

'(((((2 4 6 7 8 9 10 11 12)
     ((1 (tree prog_0_F1 0 3 vars))
       (2 (tree prog_0_TopLevel 0 1 vars (1) (1)))
       (3 (tree prog_0_F1 1 3 vars))
       (4 (tree prog_0_TopLevel 0 1 vars (3) (1)))
       (5 (tree prog_0_F1 2 3 vars))
       (6 (tree prog_0_TopLevel 0 1 vars (5) (1)))
       (7 (tree prog_0_TopLevel 0 1 vars (1) (3)))
       (8 (tree prog_0_TopLevel 0 1 vars (3) (3)))
       (9 (tree prog_0_TopLevel 0 1 vars (5) (3)))
       (10 (tree prog_0_TopLevel 0 1 vars (1) (5)))
       (11 (tree prog_0_TopLevel 0 1 vars (3) (5)))
       (12 (tree prog_0_TopLevel 0 1 vars (5) (5)))))
    (features
      ((2 (gauss 0 0 4.4)) (4 (gauss 0 1 4.4))
        (6 (gauss 0 2 4.4)) (7 (gauss 1 0 4.4))
        (8 (gauss 1 1 4.4)) (9 (gauss 1 2 4.4))
        (10 (gauss 2 0 4.4)) (11 (gauss 2 1 4.4))
        (12 (gauss 2 2 4.4)))))))

(pretty-print (parse-dag+features->log-prob (caar (batch-run-inversion (list prog1) (list test-data) 'use-features))))

(define test-data2 '(gauss 0 1))

(pretty-print (batch-run-inversion (list prog1) (list test-data2)))
