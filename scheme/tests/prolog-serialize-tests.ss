(import (prolog-serialize)
        (util)
        (printing)
        (_srfi :1))

(print (pl-clause (pl-relation 'find_at_least_one 'X 'Y 'Z)
                  (pl-relation 'findall 'X 'Y 'Z)
                  (pl-relation 'length 'Z 'N)
                  (pl-relation '> 'N 0)))

;; c1_scfg(X, Trees) :- find_at_least_one(Tree, ((X = p(Z), c2_scfg(Z, Subtrees), Tree = c1_t(Subtrees)) ; (X = b, Tree = c1_t([X]))), Trees).  

(print (pl-clause (pl-relation 'c1_scfg 'X 'Trees)
                  (pl-relation 'find_at_least_one
                               'Tree
                               (pl-disj 
                                 (pl-conj (pl-relation '= 'X (pl-relation 'p 'Z))
                                          (pl-relation 'c2_scfg 'Z 'Subtrees)
                                          (pl-relation '= 'Tree (pl-relation 'c1_t 'Subtrees))
                                          )
                                 (pl-conj (pl-relation '= 'X 'b)
                                          (pl-relation '= 'Tree (pl-relation 'c1_t (pl-list 'X)))
                                          ))
                               'Trees
                               )))
                                    
(print (facts->asserts (list (pl-clause (pl-relation 'find_at_least_one 'X 'Y 'Z)
                                        (pl-relation 'findall 'X 'Y 'Z)
                                        (pl-relation 'length 'Z 'N)
                                        (pl-relation '> 'N 0)) 
                             (pl-clause (pl-relation 'c1_scfg 'X 'Trees)
                                        (pl-relation 'find_at_least_one
                                                     'Tree
                                                     (pl-disj 
                                                       (pl-conj (pl-relation '= 'X (pl-relation 'p 'Z))
                                                                (pl-relation 'c2_scfg 'Z 'Subtrees)
                                                                (pl-relation '= 'Tree (pl-relation 'c1_t 'Subtrees))
                                                                )
                                                       (pl-conj (pl-relation '= 'X 'b)
                                                                (pl-relation '= 'Tree (pl-relation 'c1_t (pl-list 'X)))
                                                                ))
                                                     'Trees
                                                     )))))

