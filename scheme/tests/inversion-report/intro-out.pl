[s5]

dag(s1, tree(pF2, 0, 1, vars(node(node(2))))).
dag(s2, tree(pF1, 1, 2, vars)).
dag(s3, tree(pF1, 0, 2, vars, [s2])).
dag(s4, tree(pF1, 0, 2, vars, [s3])).
dag(s5, tree(pTopLevel, 0, 1, vars, [s4], [s1])).
