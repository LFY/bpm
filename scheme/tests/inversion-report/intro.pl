pTopLevel(TopLevel_res, TreeIDs) :- 
        (find_at_least_one(TreeID, 
            (pF2(X5, TopLevel_res, SubTrIDs0), 
                pF1(X5, SubTrIDs1), 
                =(Tree, 
                    tree(pTopLevel, 0, 1, vars, SubTrIDs1, SubTrIDs0)), 
                add_if_not_present(Tree, TreeID)), TreeIDs)).

pF1(F1_res, TreeIDs) :- 
    (find_at_least_one(TreeID, 
        ((=(F1_res, node(X6)), pF1(X6, SubTrIDs2), 
            =(Tree, tree(pF1, 0, 2, vars, SubTrIDs2)), 
            add_if_not_present(Tree, TreeID)); 
        (=(F1_res, 2), 
            =(Tree, tree(pF1, 1, 2, vars)), 
            add_if_not_present(Tree, TreeID))), TreeIDs)).

pF2(V0, F2_res, TreeIDs) :- 
    ((=(F2_res, node(node(V0), node(V0))), 
        find_at_least_one(TreeID, 
            (=(F2_res, node(node(V0), node(V0))), 
                =(Tree, tree(pF2, 0, 1, vars(V0))), 
                add_if_not_present(Tree, TreeID)), TreeIDs))).

go :- 
    (pTopLevel(
        node(
            node(node(node(2))), 
            node(node(node(2)))), Result), write_dags(Result)).

