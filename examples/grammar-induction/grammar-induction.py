import sys
from pyxml2prog import induce_grammar

num_args = 11

args = map(lambda i: 'auto', range(num_args))
args_in = sys.argv[1:]

arg_given = lambda i: i < len(args_in)

print args_in

type_map = {
        0 : lambda x: x, # filename
        1 : float, # model_scale
        2 : int, # stop_number
        3 : int, # beam_size
        4 : float, # prior_likelihood_weight_ratio
        5 : float, # prior_parameter
        6 : int, # strategy
        7 : int, # num_threads
        8 : float, # model_spacing
        9 : float, # num_models_to_dispay
        10 : int, # reconstitue?
        }

for i in range(num_args):
    if arg_given(i):
        print i, args_in[i]
        args[i] = type_map[i](args_in[i])

print args
induce_grammar(*args)
