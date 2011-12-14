import sys
import subprocess as sp

from configuration import ssax_dir
from configuration import src_location
from configuration import scheme_exe

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

preprocess_dae_script = src_location + "process-dae.scm"
postprocess_dae_script = src_location + "rebuild-dae.scm"

run_sxml_cmd = lambda script: delimit(" ", [ssax_interp] + map(lambda x: ssax_dir + x, ssax_deps) + [script])

make_args = lambda *args: "`echo '(main (list \"myself\" %s))' > tmp.scm && echo tmp.scm`" % (reduce(lambda x, y: x + " " + y, map(lambda a: "\"%s\"" % a, args)))

run_sxml = lambda script, *args: run_sxml_cmd(script) + " " + make_args(*args)

def induce_grammar(in_file, model_scale, beam_size = 1, likelihood_weight = 1.0, prior_weight = 1.0, prior_parameter = 1.0, num_threads = 8, model_spacing = 'auto', num_models_to_display = 'auto', reconstitute = 'auto'):
    if model_spacing == 'auto':
        model_spacing = model_scale * 50
    if num_models_to_display == 'auto':
        num_models_to_display = 8
    if reconstitute == 'auto':
        reconstitute = 0
    param_str = '_lgcg_' + reduce(lambda x, y: x + "_" + y, map(str, [model_scale, beam_size, likelihood_weight, prior_weight, prior_parameter, num_threads, model_spacing, num_models_to_display]))
    print param_str
    out_prog = in_file + param_str + ".ss"
    sp.call(run_sxml(preprocess_dae_script, in_file, out_prog, model_scale, beam_size, likelihood_weight, prior_weight, prior_parameter, num_threads, model_spacing, num_models_to_display, reconstitute, param_str), shell=True)
    sp.call("%s --script %s | tee %s.log" % (scheme_exe, out_prog, out_prog.rstrip('.ss')), shell=True)
    sp.call("%s --script %s%s.grammar.ss" % (scheme_exe, in_file, param_str), shell=True)

def rebuild_dae(orig_dae, sxml_output, out_dae):
    sp.call(run_sxml(postprocess_dae_script, orig_dae, sxml_output, out_dae), shell=True)

