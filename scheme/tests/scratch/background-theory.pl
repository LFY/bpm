:- table_all.

equals(X, Y) :- equals(Y, X).
equals(X, Y) :- equals(X, Z), equals(Z, Y).

greater(X, Y) :- greater(X, Z), greater(Z, Y).
greater(X, Y) :- equals(X, Z), greater(Z, Y).
greater(X, Y) :- equals(Y, Z), greater(X, Z).

neg(X, Y) :- neg(Y, X).
neg(X, Y) :- equals(X, Z), neg(Z, Y).
neg(X, Y) :- equals(Y, Z), neg(X, Z).

offby1(X, Y) :- offby1(Y, X).
offby1(X, Y) :- equals(X, Z), offby1(Z, Y).
offby1(X, Y) :- equals(Y, Z), offby1(X, Z).

offby2(X, Y) :- offby2(Y, X).
offby2(X, Y) :- equals(X, Z), offby2(Z, Y).
offby2(X, Y) :- equals(Y, Z), offby2(X, Z).

offby3(X, Y) :- offby3(Y, X).
offby3(X, Y) :- equals(X, Z), offby3(Z, Y).
offby3(X, Y) :- equals(Y, Z), offby3(X, Z).
