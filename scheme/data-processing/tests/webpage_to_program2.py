import sys
import subprocess as sp
import string

run = lambda c: sp.call(c, shell=True)
python = lambda c: run("python %s" % c)
scheme = lambda c: run("ikarus --r6rs-script %s" % c)
wget = lambda c: run("wget %s" % c)

# pipeline: wget -> python -> petite scheme -> ikarus scheme

page = sys.argv[1]

wget("-O page.tmp.xml http://bento.stanford.edu/bento/bento.php?url=%s" % page)
python("postprocess-bento.py page.tmp.xml > page.cleaned.tmp.xml")
python("run_sxml.py page.cleaned.tmp.xml")
scheme("output.ss > %s.results.ss" % page.lstrip("http://").translate(string.maketrans("/", "-")))


