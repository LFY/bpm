(import (rnrs)
        (bayes-program-merging)
        (printing))

;; Recursion test.

(define test-data
  (list

    '(asm (data)
          (model (data)
                 ))
    '(asm (data)
          (asm (data)
               (model (data))))
    '(asm (data)
          (asm (data)
               (asm (data)
                    (asm (data)
                         (model (data)
                                )))))

    '(asm (data)
          (asm (data)
               (asm (data)
                    (asm (data)
          (asm (data)
               (asm (data)
                    (asm (data)
                         (model (data)
                                ))))))))
    ))

(pretty-print (bpm test-data 100 1.0 2.0))

