(import (printing)
        (scene-graphs))

(define test-prog 
  '(program
     ((abstraction F13 ()
                   (choose (elem "red") (elem "red" (tr "up5" (F13)))))
      (abstraction F10 ()
                   (choose (elem "blue") (elem "blue" (tr "up5" (F10))))))
     (lambda () (choose (F13) (F10)))
     ((-1.3862943611198908 -0.287682072451781)
      (-1.3862943611198908 -0.287682072451781)
      (-0.6931471805599454 -0.6931471805599454))))

(pretty-print (tie-parameters-to-choices test-prog))

(pretty-print (sample-grammar+parameters test-prog))
(pretty-print (sample-grammar+parameters test-prog))
(pretty-print (sample-grammar+parameters test-prog))
(pretty-print (sample-grammar+parameters test-prog))
(pretty-print (sample-grammar+parameters test-prog))
(pretty-print (sample-grammar+parameters test-prog))
