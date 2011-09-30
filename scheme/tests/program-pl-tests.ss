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
  (program->pl prog1 "asdf"))

(print "Original program (expressing shared structure):")
(pretty-print (program->sexpr prog1))
(newline)

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

(define test-chart '((s6)
  ((s1 (tree pF2 0 1 (vars (node (node 1)))))
    (s2 (tree pF1 1 2 vars)) 
    (s3 (tree pF1 2 2 vars))
    (s4 (tree pF1 0 2 vars (s2 s3)))
    (s5 (tree pF1 0 2 vars (s4)))
    (s6 (tree pTopLevel 0 1 vars (s5) (s1))))))

(print (exec-chart->log-prob test-chart))

(pretty-print (batch-run-inversion (list prog1)
                            (list 
                              '(node
                                 (node (node (node (node 2))))
                                 (node (node (node (node 2)))))
                              '(node
                                 (node (node 2))
                                 (node (node 2))) 
                              '(node
                                 (node (node (node 2)))
                                 (node (node (node 2)))))))

