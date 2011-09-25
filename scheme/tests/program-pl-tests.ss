(import (program-pl)
        (printing)
        (program))

(define prog1 (make-program (list (make-named-abstraction 'F1
                                                                            '(choose (node (F1)) 2)
                                                                            '())
                                                    (make-named-abstraction 'F2
                                                                            '((lambda (V1)
                                                                                (node (gauss V1 0.1)
                                                                                      (gauss V0 0.2)
                                                                                      (choose 5 6)))
                                                                              V0)
                                                                            '(V0))
                                                    (make-named-abstraction 'F3
                                                                            '(choose (node (choose V2 1) V2) 3)
                                                                            '(V2)))
                                              '(lambda () (node (F2 (F1)) 
                                                                (F2 5)))))

(define prog1-relations (program->pl prog1))

(system "rm program-pl-testing.pl")
(with-output-to-file "program-pl-testing.pl" 
                     (lambda () (for-each (lambda (r)
                                            (print r)
                                            (newline))
                                          prog1-relations))
                     )
