import sys
import os
import subprocess as sp
import time

run_one = lambda x, y: sp.call("graffle.sh PNG %s %s" % (x, y), shell=True)

for filename in os.listdir('.'):
    if filename.endswith('.graffle'):
        run_one(filename, filename + ".png")
        time.sleep(2)

