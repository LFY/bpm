import sys
import subprocess as sp

from random import *

from concepts import *

from label_hypotheses import *

from plot_scores import plot_csv

# For generating feature induction test cases from concepts

script_header = """
(import (rnrs)
        (concept-testing)
        (_srfi :1)
        (printing)
        )
"""

def data(concept, v=0.1):
    return map(lambda x: concept(v), range(10))

var_to_name = lambda var: "data-var-%f" % var

def delimit(c, xs):
    return reduce(lambda x, y: x + c + y, xs)

def make_data_def(var, name, d):

    def mkdef(name, s):
        return "(define %s %s)" % (name, s)

    def induction_call(name, score_name):
        s1 = "(print \"feature induction with variance %f\")" % var
        s2 = "(feature-induction-n-iter 50 simple-soft-predicates %s \'() %s)" % (name, score_name)
        return s1 + "\n" + s2

    def format_list_of_floats(xs):
        return "(list " + reduce(lambda x, y: x + " " + y, map(str, xs)) + " )"
    def format_list_of_list_of_floats(xss):
        return "(list " + reduce(lambda x, y: x + "\n" + y, map(lambda xs: format_list_of_floats(xs), xss)) + " )"

    return mkdef(name, format_list_of_list_of_floats(d))

def make_footer(names, csv_name, hyp_name):
    data_name_list = reduce(lambda x, y: x + "\n" + y,
            map(lambda n: "(list %s \'%s)" % (n, n), names))

    return """
    (define results (run-multiple-data (list %s)))
    (define all-hypotheses (first results))
    (define all-scores (second results))

    (with-output-to-file \"%s\" (lambda () (display (format-csv all-scores replacement-fx))))
    (with-output-to-file \"%s\" (lambda () (display (delimit-with-formatter format-named-hypothesis "\\n" all-hypotheses))))""" % (data_name_list, csv_name, hyp_name)

if __name__ == "__main__":
    basenames = map(lambda x: x[0], all_concept_facts)
    concepts_tested = map(lambda x: x[1], all_concept_facts)
    concept_facts = map(lambda x: x[2], all_concept_facts)

    script_names = map(lambda s: s + ".scm", basenames)
    csv_names = map(lambda s: s + ".csv", basenames)
    hyp_names = map(lambda s: s + ".hyp", basenames)

    variances = map(float, sys.argv[1:])

    def run_concept(concept, concept_facts, script_name, csv_name, hyp_name):

        print "Running concept: %s %s %s" % (script_name, csv_name, hyp_name)

        var_name_datas = map(lambda v: (v, var_to_name(v), data(concept, v)), variances)

        data_defs = delimit('\n', map(lambda args: make_data_def(*args), var_name_datas))

        names = map(lambda args: args[1], var_name_datas)

        footer = make_footer(names, csv_name, hyp_name)

        final = reduce(lambda x, y: x + "\n" + y,
                [script_header, data_defs, footer])

        fh = open(script_name, "w")
        fh.write(final)
        fh.close()

        sp.call("rm %s" % (csv_name), shell=True)
        sp.call("rm %s" % (hyp_name), shell=True)
        sp.call("ikarus --r6rs-script %s" % (script_name), shell=True)

        logical_consistency_performance = process_hyp_file(background_theory, concept_facts, hyp_name)

        logic_name = hyp_name + ".csv"

        fh = open(logic_name, "w")
        fh.write(logical_consistency_performance)
        fh.close()

        plot_csv(csv_name, "log likelihood", csv_name + ".pdf", csv_name)
        plot_csv(logic_name, "logical consistency", logic_name + ".pdf", logic_name, True)


    map(lambda args: run_concept(*args), zip(concepts_tested, concept_facts, script_names, csv_names, hyp_names))



