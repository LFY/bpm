import sys
import subprocess as sp
import string

from commands import *

from bento2bpm import bento2bpm

def page2prog_remote(page, outname):
    wget("-O page.tmp.xml http://bento.stanford.edu/bento/bento.php?url=%s" % page)
    page2prog_local("page.tmp.xml", outname)

def page2prog_local(xml, outname):
    scheme_script = outname + ".run.ss" 
    bento2bpm(xml, scheme_script)
    scheme("%s > %s" % (scheme_script, outname))

