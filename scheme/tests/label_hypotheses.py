
from prolog_pickle import *

def delimit(c, xs):
    return reduce(lambda x, y: x + c + y, xs)

def delim_select(c, line, i):
    return line.strip().split(c)[i]

def delim_select(c, line):
    return line.strip().split(c)

def parse_hyps(named_hyp_file):
    fh = open(named_hyp_file)
    named_hyps = fh.readlines()

    hyp_dict = {}

    curr_name = ""
    curr_predicate = ""
    curr_args = ""

    for line in named_hyps:
        if line.startswith("Name:"):
            curr_name = delim_select(" ", line)[1]
        else:
            curr_predicate = delim_select(" ", line)[0]
            curr_args = delim_select(" ", line)[1:]
            hyp_dict[curr_name] = hyp_dict.get(curr_name, []) + [(curr_predicate, curr_args)]

    return hyp_dict

def idx_to_var(idx):
    return "x" + idx

def make_run_stmts(pred_idxs):
    pred_name_map = {
            'soft-greater?' : 'greater',
            'soft-neg?' : 'neg',
            'soft-offby1?' : 'offby1',
            'soft-offby2?' : 'offby2',
            'soft-offby3?' : 'offby3',
            'soft-eq?' : 'equals'}

    renamed_pred_idx = map(lambda (p, idx): (pred_name_map[p], map(idx_to_var, idx)), pred_idxs)

    run_one = lambda s: "run(%s)" % s

    pred_idx_to_app = lambda (p, idx): "%s(%s)" % (p, delimit(",", idx))

    result_body = delimit(",", map(run_one, map(pred_idx_to_app, renamed_pred_idx))) + "."

    return result_body


def check_horn_clause(background_theory, concept_theory, pred_idxs):
    pl_script = ""
    pl_script += background_theory + concept_theory

    pl_script += "run(X) :- X->(write(\'result: success\'), nl);(write(\'result: failure\'), nl).\n"

    pl_script += "go :- %s" % make_run_stmts(pred_idxs)

    return run_prolog(pl_script)

def check_horn_clauses(background_theory, concept_theory, hyp_dict):
    return dict(map(lambda (name, hyp): (name, check_horn_clause(background_theory, concept_theory, hyp)), hyp_dict.items()))

def format_csv(cols):
    rows = zip(*cols)
    process_one_row = lambda r: delimit('\t', map(lambda v: str(v) if type(v) != str else v, r))
    return delimit('\n', map(process_one_row, rows))

def process_hyp_file(background_theory, concept_theory, hyp_name):
    hyp_dict = parse_hyps(hyp_name)
    hyp_check = check_horn_clauses(background_theory, concept_theory, hyp_dict)

    cols = sorted(map(lambda (k, vs): [k] + map(lambda v: 1 if v else 0, vs), hyp_check.items()), key = lambda x: x[0])

    max_col_len = max(map(len, cols))

    xcol = ["Iteration"] + range(max_col_len)

    return format_csv([xcol] + cols)
