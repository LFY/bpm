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

