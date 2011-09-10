import os, sys, pickle, random
from subprocess import Popen, PIPE
from pyscenegraph import *

def run_cmd(cmd):
    p = Popen(cmd, shell=True, stdout=PIPE, stderr=PIPE)
    stdout, stderr = p.communicate()
    return stdout

if __name__ == "__main__":
    args=pickle.load(sys.stdin)
    debug = open("debug.txt", 'w')
    print >>debug, args

    xs = args[0]
    ys = args[1]

    (minx, miny), (maxx, maxy) = args[2]

    xline = lambda x: polygon('line', [(x, miny), (x, maxy)])
    yline = lambda y: polygon('line', [(minx, y), (maxx, y)])

    all_nodes = map(xline, xs) + map(yline, ys)

    if len(all_nodes) > 0:
        final = asm('all', map(xline, xs) + map(yline, ys))
        saveSG(final, 'gridlines.svg')

    pickle.dump(args, sys.stdout)
