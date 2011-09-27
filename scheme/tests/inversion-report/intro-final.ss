((relation pTopLevel (TopLevel_res TreeIDs)
   ((find_at_least_one TreeID
      (conj (pF2 X3 TopLevel_res SubTrIDs0)
        (pF1 X3 SubTrIDs1)
        (= Tree
          (tree pTopLevel 0 1 (vars) SubTrIDs1 SubTrIDs0))
        (add_if_not_present Tree TreeID))
      TreeIDs)))
  (relation pF1 (F1_res TreeIDs)
    ((find_at_least_one TreeID
       (disj
         (conj (= F1_res (node X4)) (pF1 X4 SubTrIDs2)
           (= Tree (tree pF1 0 2 (vars) SubTrIDs2))
           (add_if_not_present Tree TreeID))
         (conj (= F1_res 2) (= Tree (tree pF1 1 2 (vars)))
           (add_if_not_present Tree TreeID)))
       TreeIDs)))
  (relation pF2 (V0 F2_res TreeIDs)
    (conj (= F2_res ((node (node V0) (node V0))))
      (find_at_least_one TreeID
        (conj (= F2_res ((node (node V0) (node V0))))
          (= Tree (tree pF2 0 1 (vars V0)))
          (add_if_not_present Tree TreeID))
        TreeIDs))))

