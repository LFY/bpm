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

