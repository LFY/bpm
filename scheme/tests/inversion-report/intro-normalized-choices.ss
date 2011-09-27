((abstr-eqs TopLevel () TopLevel_res
   ((= TopLevel_res (F2 X3)) (= X3 (F1))))
  (abstr-eqs F1 () F1_res
    (choose ((= F1_res (node X4)) (= X4 (F1)))
      ((= F1_res 2))))
  (abstr-eqs F2 (V0) F2_res
    ((= F2_res ((node (node V0) (node V0)))))))

