import os
import subprocess as sp

beam_widths = [100]
likelihood_weights = [1.0]
prior_weights = [0.0, 2.0, 3, 4, 5, 6, 7, 8]
scale = [1.0]

def gen_one_test(args):
    scale, beam, like, prior = args
    s = """
from pyxml2prog import *
dae2bpm("seuss_final_01.dae", %d, %d, %f, %f)""" % (scale, beam, like, prior)

    dirname = "test_%d_%d_%f_%f" % (scale, beam, like, prior)
    sp.call("mkdir %s" % dirname, shell=True)
    fh = open("%s/%s.py" % (dirname, dirname), "w")
    fh.write(s)
    fh.close()

    sp.call("cd %s; ln -s ../seuss_final_01.dae seuss_final_01.dae; cd ../" % dirname, shell=True)

    sp.call("echo \"cd %s; python %s.py &\" >> %s.sh" % (dirname, dirname, dirname), shell=True)

sp.call("rm -rf test_*", shell=True)
map(gen_one_test, [(s, b, l, p) for s in scale for b in beam_widths for l in likelihood_weights for p in prior_weights])


    





