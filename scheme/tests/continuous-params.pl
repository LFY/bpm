:- dynamic dag/2.

find_at_least_one(X, Y, Z) :- findall(X, Y, Z), length(Z, N), N > 0.

add_if_not_present(Term, ID) :- (dag(ID, Term)) -> (true) ; (gensym(s, ID), assertz(dag(ID, Term))).

write_dags(TopLevel) :- write(TopLevel), listing(dag).

pTopLevel(TopLevel_res, TreeIDs) :- 
    (find_at_least_one(TreeID, 
        (=(TopLevel_res, gauss(X0, X1)), 
            pF1(X0, SubTrIDs0), 
            pF1(X1, SubTrIDs1), 
            =(Tree, tree(pTopLevel, 0, 1, vars, SubTrIDs1, SubTrIDs0)), 
            add_if_not_present(Tree, TreeID)), TreeIDs)).

pF1(F1_res, TreeIDs) :- 
    ((=(F1_res, 0)); 
            (=(F1_res, 1)); 
                (=(F1_res, 2))),
    (find_at_least_one(TreeID, 
        ((=(F1_res, 0), =(Tree, tree(pF1, 0, 3, vars)), add_if_not_present(Tree, TreeID)); 
            (=(F1_res, 1), =(Tree, tree(pF1, 1, 3, vars)), add_if_not_present(Tree, TreeID)); 
                (=(F1_res, 2), =(Tree, tree(pF1, 2, 3, vars)), add_if_not_present(Tree, TreeID))), 
                    TreeIDs)).

go :- (pTopLevel(gauss(0, 1), Result), write_dags(Result)).

go2 :- (pTopLevel2(4.4, Result), write_dags(Result), listing(gauss)).

:- dynamic gauss/4.

pTopLevel2(TopLevel_res, TreeIDs) :- 
    (find_at_least_one(TreeID, 
        (
            %=(TopLevel_res, gauss(X0, X1)), 
            pF1(X0, SubTrIDs0), 
            pF1(X1, SubTrIDs1), 
            =(Tree, tree(pTopLevel, 0, 1, vars, 
                SubTrIDs1, SubTrIDs0)), 
            add_if_not_present(Tree, TreeID),
            assertz(gauss(X0, X1, TopLevel_res, TreeID))
        
        ), TreeIDs)).


