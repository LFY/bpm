import subprocess as sp
from configuration import scheme_exe

run = lambda c: sp.call(c, shell=True)
python = lambda c: run("python %s" % c)
scheme = lambda c: run("%s --r6rs-script %s" % (scheme_exe, c))
wget = lambda c: run("wget %s" % c)

