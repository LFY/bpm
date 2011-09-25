(import (program-pl)
        (printing)
        (program)
        (prolog-serialize))

(define prog1 
  (make-program 
    (list 
      (make-named-abstraction 'F1
                              '(choose (node (F1)) 2)
                              '())
      (make-named-abstraction 'F2
                              '((lambda (V1)
                                  (node (node V1)
                                        (node V0))) V0)
                              '(V0)))
    '(lambda () (F2 (F1)) 
       )))

(define prog1-relations 
  (program->pl prog1 'no-trees))

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

