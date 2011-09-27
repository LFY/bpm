((define (TopLevel)
   (let ((TopLevel_res (let ((X3 (F1))) (F2 X3))))
     TopLevel_res))
  (define (F1)
    (let ((F1_res
           (choose
             (let ((X5 (let ((X4 (F1))) (node X4)))) X5) 2)))
      F1_res))
  (define (F2 V0)
    (let ((F2_res ((node (node V0) (node V0))))) F2_res)))

