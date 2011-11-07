(import (printing)
        (_srfi :1))

;; lexicographic ordering of grammars is used when we compare grammars and
;; their likelihoods and see if they are duplicates (in the part of beam search
;; when we are merging duplicate grammars)

;; that can definitely be optimized more (i.e., only calculate likelihood for
;; grammars that are unique after being lexicographically ordered)

;; but, for now, assume that the order in which rules occur in the nonterminal
;; definition are the order in which the parameters are specified.

;; Example:

;; Before, our 'grammar' was a tagged list with (program <list of abstractions> <starting rule>)

'(program
   ((abstraction F82 ()
      (choose (elem "node" (tr "forward" (F82)))
        (elem "node")))
     (abstraction F77 ()
       (choose
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82))))))
   (lambda () (choose (F77) (F77) (F77) (F77) (F77) (F77))))

;; Now, we want to store the parameters too. The plan is to store them in the
;; same order as the abstractions, ending with the parameters of the start
;; symbol.

'(program
   ((abstraction F82 ()
      (choose (elem "node" (tr "forward" (F82)))
        (elem "node")))
     (abstraction F77 ()
       (choose
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82)))
         (elem "root" (tr "right" (F82)) (tr "left" (F82))))))
   (lambda () (choose (F77) (F77) (F77) (F77) (F77) (F77)))

   ;; the corresponding parameters: 
   ((0.7 0.3) ;; for F82
    (1.0 0.0 0.0 0.0 0.0 0.0) ;; for F77
    (1.0 0.0 0.0 0.0 0.0 0.0) ;; for the start symbol
    ))

;; Then, the parameters can be used in sampling the grammar, in addition to
;; calculating the probability of any one example
   

