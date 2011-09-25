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

