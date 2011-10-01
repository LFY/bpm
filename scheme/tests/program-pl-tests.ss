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
                              '(choose (node (F1)) 2)
                              '())
      (make-named-abstraction 'F2
                              '(
                                  (node (node V0)
                                        (node V0))) 
                              '(V0)))
    '(lambda () (F2 (F1)) 
       )))

(define prog1-relations 
  (program->pl prog1))

(print "Original program (expressing shared structure):")
(pretty-print (program->sexpr prog1))
(newline)

(print "Query data:")
(pretty-print '(node
                 (node (node (node 2)))
                 (node (node (node 2)))))

(print "Rewritten as logic program:")

(display-pl prog1-relations)
(newline)

(define prog1-relations-trees 
  (program->pl prog1))

(print "Sample query (successful):")
(display-pl (list (pl-clause (pl-relation 'go)
                             (pl-relation 'pTopLevel
                                          '(node
                                               (node (node (node 2)))
                                               (node (node (node 2))))))))
(newline)

(print "Sample query (failing; the subtrees are not the same, though they each match F1):")
(display-pl (list (pl-clause (pl-relation 'go)
                             (pl-relation 'pTopLevel
                                          '(node
                                               (node (node (node 2)))
                                               (node (node 2)))))))
(newline)

(print "Rewritten as proof-tree-building logic program:")

(display-pl prog1-relations-trees)
(newline)

(print "Sample query:")
(display-pl (list (pl-clause (pl-relation 'go)
                             (pl-relation 'pTopLevel
                                          '(node
                                               (node (node (node 2)))
                                               (node (node (node 2))))
                                          'Result)
                             (pl-relation 'write_dags
                                          'Result))))
(newline)

(print "The resulting parse dag:")
(pretty-print (batch-run-inversion (list prog1) (list '(node (node (node (node 2))) (node (node (node 2)))))))
(pretty-print (exec-chart->log-prob (caar (batch-run-inversion (list prog1) (list '(node (node (node (node 2))) (node (node (node 2)))))))))

(print "For program with many different execution paths for same data:")

(define prog2 
  (make-program 
    (list 
      (make-named-abstraction 'F1
                              '(choose (node (F1)) 
                                       (node (node (F1)))
                                       (node end)
                                       (node (node end)))
                              '())
      )
    '(lambda () (F1) 
       )))

(pretty-print (program->sexpr prog2))

(print "Query data:")

(pretty-print '(node (node (node (node end)))))

(print "Resulting parse DAGs:")

(define prog2-dags (batch-run-inversion (list prog2)
                                   (list 
                                     '(node (node (node (node end))))
                                         )))

(pretty-print prog2-dags)
(print "Probability:")
(print (exec-chart->log-prob (caar prog2-dags)))
