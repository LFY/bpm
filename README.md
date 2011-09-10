# Program merging

# Requred dependencies

1. Ikarus or Vicare Scheme
2. SWI-Prolog 

For generating programs from webpages using the Bento webpage segmentation
service:

3. Petite Chez Scheme
4. Python

# Installation steps

0. Install the above dependencies.

1. Go to pyxml2prog directory. Issue
    
        sh install.sh

# How to use

## Scheme interface

Given a list of S-expressions, run the program merging algorithm by issuing

    (learn-model 
        (the list of S-expressions) 
        (beam search width) 
        (initial counter to track depth))

The algorithm will stop when the learned program remains the same for 5
programs in a row.

See examples/sg-learn.ss for an example on a scene graph.

## Python interface (for webpage layout analysis)

The overall workflow is around the pyxml2prog package. It contains functions to
process XML files into Scheme programs. 

See examples/bento_examples.py for details.

# Organization

A dependency graph for major parts of the algorithm. 

- beam-learning.ss: the search algorithm, learn-model interface
    - abstract.ss: program merging moves
        - inverse-inline.ss: Inventing new abstractions
            - unification.ss: Unification and anti-unification algorithms
        - dearguments.ss: Removing arguments from existing abstractions
    - program-likelihood.ss: Calculating likelihood of data given program
        - chart-parsing.ss: Chart parser (uses SWI-Prolog)
            - named-search-trees.ss: Converting program into SCFG




