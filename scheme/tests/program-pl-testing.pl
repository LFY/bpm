:- dynamic dag/2.

find_at_least_one(X, Y, Z) :- findall(X, Y, Z), length(Z, N), N > 0.

add_if_not_present(Term, ID) :- (dag(ID, Term)) -> (true) ; (gensym(s, ID), assertz(dag(ID, Term))).

       write_dags(TopLevel) :- find_at_least_one(DagNode, (dag(ID, Tree), retract(dag(ID, Tree)), DagNode = [ID, Tree]), DagNodes), term2sexpr([TopLevel, DagNodes]).

       open_paren :- write('(').
       close_paren :- write(')').

       term2sexpr([X|Xs]) :- write('('), term2sexpr(X), map_term2sexpr(Xs), write(') ').
       term2sexpr(T) :- T =.. L, L = [F|[]], write(F), write(' ').
       term2sexpr(T) :- T =.. L, L = [F|Xs], write('('), write(F), write(' '), map_term2sexpr(Xs), write(') ').

       map_term2sexpr([]) :- true.
       map_term2sexpr([X|Xs]) :- term2sexpr(X), map_term2sexpr(Xs).

pTopLevel(TopLevel_res, TreeIDs) :- 
    (find_at_least_one(TreeID, 
        (pF2(X3, TopLevel_res, SubTrIDs0), 
            pF1(X3, SubTrIDs1), 
            =(Tree, tree(pTopLevel, 0, 1, vars, SubTrIDs1, SubTrIDs0)), 
            add_if_not_present(Tree, TreeID)), TreeIDs)).

pF1(F1_res, TreeIDs) :- 
    (find_at_least_one(TreeID, 
        ((=(F1_res, node(X4)), 
            pF1(X4, SubTrIDs2), 
            =(Tree, tree(pF1, 0, 2, vars, SubTrIDs2)), 
            add_if_not_present(Tree, TreeID)); 
            (=(F1_res, 1), =(Tree, tree(pF1, 1, 2, vars)), 
                add_if_not_present(Tree, TreeID));
            (=(F1_res, 1), =(Tree, tree(pF1, 2, 2, vars)), 
                add_if_not_present(Tree, TreeID))
            ), TreeIDs)).

pF2(V0, F2_res, TreeIDs) :- 
    ((=(F2_res, node(node(V0), node(V0))), 
        find_at_least_one(TreeID, 
        (=(F2_res, node(node(V0), node(V0))), 
            =(Tree, tree(pF2, 0, 1, vars(V0))), 
            add_if_not_present(Tree, TreeID)), TreeIDs))).

go :- (pTopLevel(node(node(node(node(1))), node(node(node(1)))), Result), write_dags(Result)).

