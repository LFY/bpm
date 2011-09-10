import sys
import subprocess as sp

from configuration import ssax_dir
from configuration import src_location

# For converting Bento XML output to Scheme programs describing program merging parameters

def preprocess_bento(fn, out):
    fh = open(fn, 'r').readlines()
    outh = open(out, 'w')

    for l in fh:
        if l.count("<!DOCTYPE") == 0:
            outh.write(l)
    

def delimit(c, xs):
    return reduce(lambda x, y: x + c + y, xs)

ssax_interp = "echo \"(exit 101)\" | petite"
ssax_deps = [
        "myenv-chez.scm",
        "ppretty-prints.scm",
        "SXML-tree-trans.scm",
        "srfi-13-local.scm",
        "char-encoding.scm",
        "util.scm",
        "look-for-str.scm",
        "input-parse.scm",
        "SXPath-old.scm",
        "SSAX-code.scm"]

run_sxml_script = src_location + "process-bento.scm"

run_sxml_cmd = delimit(" ", [ssax_interp] + map(lambda x: ssax_dir + x, ssax_deps) + [run_sxml_script])
make_run_sxml = lambda arg, out: "`echo '(main (list \"myself\" \"%s\" \"%s\"))' > tmp.scm && echo tmp.scm`" % (arg, out,)

def bento2bpm(in_file, out_prog):

    fn = in_file + ".processed"

    preprocess_bento(in_file, fn)

    final_cmd = run_sxml_cmd + " " + make_run_sxml(fn, out_prog)

    print final_cmd

    sp.call(run_sxml_cmd + " " +  make_run_sxml(fn, out_prog), shell=True)


