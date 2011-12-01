import sys
from pyxml2prog import induce_grammar

num_args = 9

args = map(lambda i: 'auto', range(num_args))
args_in = sys.argv[1:]

arg_given = lambda i: i < len(args_in)

print args_in

type_map = {
        0 : lambda x: x, # filename
        1 : float, # model_scale
        2 : int, # beam_size
        3 : float, # likelihood_weight
        4 : float, # prior_weight
        5 : float, # prior_parameter
        6 : int, # num_threads
        7 : float, # model_spacing
        8 : float, # num_models_to_dispay
        }

for i in range(num_args):
    if arg_given(i):
        args[i] = type_map[i](args_in[i])

print args
induce_grammar(*args)
