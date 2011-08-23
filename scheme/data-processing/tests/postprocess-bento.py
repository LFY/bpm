import sys

fn = sys.argv[1]

fh = open(fn, 'r').readlines()

for l in fh:
    if l.count("<!DOCTYPE") == 0:
        sys.stdout.write(l)

