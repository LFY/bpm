import sys
from subprocess import Popen, PIPE

def run_cmd(cmd):
    p = Popen(cmd, shell=True, stdout=PIPE, stderr=PIPE)
    stdout, stderr = p.communicate()
    return stdout

def parse_results(output):
    line = map(lambda l: l.strip().split(" ")[1], filter(lambda s: s.startswith("result:"), output.split("\n")))
    res_str_to_bool = lambda s: True if s == "success" else False
    return map(res_str_to_bool, line)

def run_prolog(prog):

    tmp_name = "run.tmp.pl"
    fh = open(tmp_name, "w")
    fh.write(prog)
    fh.close()

    results = run_cmd("bp -g \"consult(\\\"%s\\\"),go\"" % tmp_name)

    return parse_results(results)

test_prog = """
:- table_all.

greater(X, Y) :- greater(X, Z), greater(Z, Y).

equals(X, Y) :- equals(Y, X).
equals(X, Y) :- equals(X, Z), equals(Z, Y).

greater(X, Y) :- equals(X, Z), greater(Z, Y).
greater(X, Y) :- equals(Y, Z), greater(X, Z).

greater(a, b).
greater(b, c).
equals(c, d).

top :- greater(a, d).

go :- top->(write('result: success'), nl);(write('result: failure'), nl).
"""

def test():
    print run_prolog(test_prog)

