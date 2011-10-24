(import (printing)
        (grammar-induction)
        (scene-graphs))

;; Test data, as a list of S-expressions with elem, tr as constructors

(define data
  (list
    '(elem "red")
    '(elem "red")
    '(elem "blue")
    '(elem "blue")
    '(elem "blue")
    '(elem "blue")
    '(elem "blue")
    '(elem "blue")
    '(elem "blue")
    '(elem "blue")
     ))

;; The interface to grammar induction
(define grammar
  (let* ([beam-width 10]
         [likelihood-weight 1.0]
         [prior-weight 0.01])
    (gi-bmm-prototype
      data 
      beam-width
      likelihood-weight
      prior-weight)))
        
(pretty-print grammar)

;; Sampling the grammar
(print "Samples from the grammar:")

(pretty-print (sample-grammar grammar))
(pretty-print (sample-grammar grammar))
(pretty-print (sample-grammar grammar))
