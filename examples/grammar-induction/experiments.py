from pyxml2prog import *

# Change: # threads and what models to run
threads = 2
models_scales = [('seuss_examples.dae', 1), ('sakura_examples.dae', 10), ('playground_examples.dae', 100), ('scary_examples.dae', 1), ('space_examples.dae', 3), ('ships_examples.dae', 10), ('castle_examples.dae', 5)]

# Shouldn't need to change the other stuff

biggest_examples = [('castle_examples.dae', 5), ('playground_examples.dae', 100)]
small_examples = [('seuss_examples.dae', 1), ('sakura_examples.dae', 10),('scary_examples.dae', 1), ('space_examples.dae', 3), ('ships_examples.dae', 10)]

alphas = [1.0]

likelihood_priors = [(1.0, 1.0), (10.0, 1.0), (1.0, 10.0), (2.0, 1.0), (1.0, 2.0)]

beam_widths = [10, 100, 1000]


[induce_grammar(model, scale, beam_width, likelihood, prior, alpha, threads) for (model, scale) in models_scales for beam_width in beam_widths for alpha in alphas for (likelihood, prior) in likelihood_priors 
        if (((model, scale) in biggest_examples) and (beam_width == 10)) or # Put in further experiment conditions here
        (((model, scale) in small_examples))]
