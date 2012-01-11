(import (printing)
        (grammar-induction)
        (scene-graphs))

;; Test data, as a list of S-expressions with elem, tr as constructors

(define data
  (list
    '(elem "red" 
           (tr "up5" 
               (elem "red" 
                     (tr "up5" 
                         (elem "red" 
                               (tr "up5" 
                                   (elem "red")))))))
    '(elem "blue" 
           (tr "up5" 
               (elem "blue" 
                     (tr "up5" 
                         (elem "blue" 
                               (tr "up5" 
                                   (elem "blue")))))))
    ))

;; The interface to grammar induction
(define grammar
  (let* ([beam-width 10]
         [likelihood-weight 1.0]
         [prior-weight 1.0])
    (gi-bmm data 
            50
            50
            1.0
            1.0
            1.0
            4
            2
            #f)))
        
(pretty-print grammar)

;; Sampling the grammar
(print "Samples from the grammar:")

(pretty-print (sample-grammar grammar))
(pretty-print (sample-grammar grammar))
(pretty-print (sample-grammar grammar))
