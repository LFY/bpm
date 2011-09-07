import os, sys, pickle, random
from subprocess import Popen, PIPE

def run_cmd(cmd):
    p = Popen(cmd, shell=True, stdout=PIPE, stderr=PIPE)
    stdout, stderr = p.communicate()
    return stdout

if __name__ == "__main__":
    args=pickle.load(sys.stdin)
    args = run_cmd("swipl -qs chart-parsing-header.pl -t \"%s\"" % args)
    pickle.dump(args.rstrip('\n'),sys.stdout)
