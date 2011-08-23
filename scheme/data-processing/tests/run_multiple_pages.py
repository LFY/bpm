import sys
import subprocess as sp

pages = sys.argv[1:]

for p in pages:
    sp.call("python webpage_to_program.py %s" % p, shell=True)
