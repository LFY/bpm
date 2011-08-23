import sys
import subprocess as sp

# For converting XML to s-expressions

def delimit(c, xs):
    return reduce(lambda x, y: x + c + y, xs)

fn = sys.argv[1]

ssax_dir = "/Users/lyang/bpm/scheme/SSAX/lib/"
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
run_sxml_script = "/Users/lyang/bpm/scheme/data-processing/run-sxml.scm"
run_sxml_cmd = delimit(" ", [ssax_interp] + map(lambda x: ssax_dir + x, ssax_deps) + [run_sxml_script])
make_run_sxml = lambda arg: "`echo '(main (list \"myself\" \"%s\"))' > tmp.scm && echo tmp.scm`" % (arg,)

final_cmd = run_sxml_cmd + " " + make_run_sxml(fn)

print final_cmd

sp.call(run_sxml_cmd + " " +  make_run_sxml(fn), shell=True)


