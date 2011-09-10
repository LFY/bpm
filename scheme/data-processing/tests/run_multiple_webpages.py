import sys, string
import subprocess as sp
import os

run = lambda c: sp.call(c, shell=True)
python = lambda c: run("python %s" % c)
scheme = lambda c: run("ikarus --r6rs-script %s" % c)
wget = lambda c: run("wget %s" % c)

page_list_fn = sys.argv[1]

pages = map(lambda s: s.rstrip('\n'), filter(lambda s: s != "\n" and s != "", open(page_list_fn, 'r').readlines()))

postprocess_script = "/Users/lyang/bpm/scheme/data-processing/tests/postprocess-bento.py"
run_sxml_script = "/Users/lyang/bpm/scheme/data-processing/tests/run_sxml.py"

prolog_header = "/Users/lyang/bpm/scheme/tests/chart-parsing-header.pl"

bento_archive_prefix = "http://bento.stanford.edu/test/archive/"

def run_one_page(page):
    run("mkdir %s" % page)
    os.chdir(page)

    wget("-O page.tmp.xml http://bento.stanford.edu/bento/bento.php?url=%s" % (bento_archive_prefix + page))

    python("%s page.tmp.xml > page.cleaned.tmp.xml" % postprocess_script)
    python("%s page.cleaned.tmp.xml" % run_sxml_script)

    run("cp %s ." % prolog_header)

    scheme("output.ss > results.ss")

    os.chdir("../")

for page in pages:
    run_one_page(page)




