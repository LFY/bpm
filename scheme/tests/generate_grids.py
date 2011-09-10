import subprocess as sp
import os
import sys

fn = sys.argv[1]

def get_last_program(fn):
    lines = open(fn, 'r').readlines()
    k = -1
    for i in range(len(lines)):
        if lines[i].startswith("(program"):
            k = i

    return "" if k == -1 else "(import (html-vis)) (html-program->gridlines '%s)" % reduce(lambda x, y: x + y, lines[k:])

fh = open("last-prog.ss", 'w')
fh.write(get_last_program(fn))
fh.close()

sp.call("ikarus --script last-prog.ss", shell=True)

