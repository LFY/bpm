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

