(import (forkmap)
        (printing)
        (_srfi :1)
        (util))

(print (forkmap (lambda (i) 
           (begin
             (length (map (lambda (x) x)
                  (iota 1000000)))))
             
           '(1 2 3 4)))

(print (forkmap
  (lambda (i) (+ i 1))
  (iota 1000)))
