find_at_least_one(X, Y, Z) :- findall(X, Y, Z), length(Z, N), N > 0.

test_data(Data) :- telling(Old), tell('chart-parse-out.ss'),
    (pred_Start(Data, Result)->term2sexpr(Result);term2sexpr([])), nl, told, tell(Old).

term2sexpr([X|Xs]) :- write('('), term2sexpr(X), map_term2sexpr(Xs), write(') ').
term2sexpr(T) :- T =.. L, L = [F|[]], write(F), write(' ').
term2sexpr(T) :- T =.. L, L = [F|Xs], write('('), write(F), write(' '), map_term2sexpr(Xs), write(') ').

map_term2sexpr([]) :- true.
map_term2sexpr([X|Xs]) :- term2sexpr(X), map_term2sexpr(Xs).


