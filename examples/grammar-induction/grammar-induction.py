import sys
from pyxml2prog import induce_grammar

model, model_scale, beam_size, likelihood_weight, prior_weight, prior_parameter, num_threads = sys.argv[1:]

print sys.argv[1]

model_scale = float(model_scale)
beam_size = int(beam_size)
likelihood_weight = float(likelihood_weight)
prior_weight = float(prior_weight)
prior_parameter = float(prior_parameter)
num_threads = int(num_threads)

induce_grammar(model, model_scale, beam_size, likelihood_weight, prior_weight, prior_parameter, num_threads)
