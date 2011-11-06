import sys
import subprocess as sp

from configuration import ssax_dir
from configuration import src_location
from configuration import scheme_exe

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

process_xml_script = src_location + "process-xml.scm"

run_sxml_cmd = lambda script: delimit(" ", [ssax_interp] + map(lambda x: ssax_dir + x, ssax_deps) + [script])

make_args = lambda *args: "`echo '(main (list \"myself\" %s))' > tmp.scm && echo tmp.scm`" % (reduce(lambda x, y: x + " " + y, map(lambda a: "\"%s\"" % a, args)))

run_sxml = lambda script, *args: run_sxml_cmd(script) + " " + make_args(*args)

def xml2sxml(in_file, out_file):
    sp.call(run_sxml(process_xml_script, in_file, out_file), shell=True)
