
(import (beam-learning)
        (_srfi :1)
        (printing))

(define data (list 
              '(node
  (data)
  (node
    (data (y 0) (x 0) (width 1280) (height 1024))
    (node
      (data (y 67) (x 50) (width 850) (height 747))
      (node
        (data (y 67) (x 50) (width 250) (height 367))
        (node (data (y 67) (x 50) (width 244) (height 118)))
        (node (data (y 238) (x 50) (width 250) (height 105))
          (node (data (y 238) (x 50) (width 66) (height 21)))
          (node (data (y 259) (x 50) (width 98) (height 21)))
          (node (data (y 280) (x 50) (width 86) (height 21)))
          (node (data (y 301) (x 50) (width 51) (height 21)))
          (node (data (y 322) (x 50) (width 79) (height 21)))))
      (node (data (y 122) (x 325) (width 575) (height 692))))
    (node (data (y 887) (x 50) (width 850) (height 123)))))))

(pretty-print (learn-model data 1 15))
