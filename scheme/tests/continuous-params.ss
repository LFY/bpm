(import (program-pl)
        (printing)
        (program)
        (prolog-serialize))

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
  (program->pl prog1))

(display-pl (append prog1-relations test-query))

