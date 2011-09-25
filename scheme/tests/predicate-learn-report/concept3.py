concept3_facts= """
neg(x0, x1).

"""

def concept3(v = 0.1):

    n = sample(range(-20, 20), 1)[0]

    x0 = gauss(n, v)
    x1 = x0 * gauss(-1, v)

    return [x0, x1]


