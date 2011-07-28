from random import *

background_theory = """
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

"""


concept1_facts = """
greater(x1, x0).
greater(x2, x1).
greater(x3, x1).
offby1(x0, x1).
offby1(x1, x2).
offby1(x2, x3).

"""

def concept1(v=0.1):
    n = sample(range(-20, 20), 1)[0]
    g = lambda : gauss(1, v)

    x0 = gauss(n, v)
    x1 = x0 + g()
    x2 = x1 + g()
    x3 = x2 + g()

    return [x0, x1, x2, x3]

concept2_facts = """
equals(x0, x1).
equals(x2, x3).
greater(x2, x0).
offby1(x2, x0).

"""

def concept2(v = 0.1):
    n= sample(range(-20, 20), 1)[0]

    x0 = gauss(n, v)
    x1 = x0 + gauss(0, v)

    x2 = x1 + gauss(1, v)
    x3 = x2 + gauss(1, v)

    return [x0, x1, x2, x3]

concept3_facts= """
neg(x0, x1).

"""

def concept3(v = 0.1):

    n = sample(range(-20, 20), 1)[0]

    x0 = gauss(n, v)
    x1 = x0 * gauss(-1, v)

    return [x0, x1]

concept4_facts = """
neg(x0, x2).
neg(x1, x3).

"""

def concept4(v = 0.1):
    n = lambda : sample(range(-20, 20), 1)[0]

    x0 = gauss(n(), v)
    x1 = gauss(n(), v)
    x2 = x0 * gauss(-1, v)
    x3 = x1 * gauss(-1, v)
    
    return [x0, x1, x2, x3]

concept5_facts = """
neg(x0, x3).
neg(x1, x2).
offby1(x0, x1).
offby1(x2, x3).
"""

def concept5(v = 0.1):
    n = sample(range(-20, 20), 1)[0]

    x1 = gauss(n, v)
    x0 = x1 + gauss(1, v)

    x2 = x1 * gauss(-1, v)
    x3 = x0 * gauss(-1, v)

    return [x0, x1, x2, x3]

concept6_facts = """
neg(x0, y).
offby1(y, x1).
greater(x1, x0).
greater(x1, y).
"""

def concept6(v = 0.1):
    n = sample(range(-20, 20), 1)[0]
    
    x0 = gauss(n, v)
    y = gauss(1, v) + x0

    x1 = y + gauss(1, v)

    return [x0, x1]

all_concept_facts = [
        ("concept1", concept1, concept1_facts),
        ("concept2", concept2, concept2_facts),
        ("concept3", concept3, concept3_facts),
        ("concept4", concept4, concept4_facts),
        ("concept5", concept5, concept5_facts),
        ("concept6", concept6, concept6_facts)]
