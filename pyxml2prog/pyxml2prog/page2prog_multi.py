import sys, string
import subprocess as sp
import os

from configuration import scheme_exe
from page2prog import *
from commands import *

def sanitize_pagename(page):
    return page.lstrip("http://").translate(string.maketrans("/", "-"))

def run_one_page(url):
    pagedir = sanitize_pagename(url)
    print "PAGE URL: %s" % url
    run("mkdir %s" % pagedir)
    os.chdir(pagedir)

    page2prog_remote(url, "results.ss")

    os.chdir("../")

def run_multiple_pages(pages):
    for page in pages:
        run_one_page(page)

def run_multiple_from_pagelist(fn):
    pages = map(lambda s: s.rstrip('\n'), filter(lambda s: s != "\n" and s != "", open(fn, 'r').readlines()))
    run_multiple_pages(pages)
