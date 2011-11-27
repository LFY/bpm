# Program merging

# Requred dependencies

1. Ikarus or Vicare Scheme
2. SWI-Prolog 

For grammar induction of 3D scenes + generating programs from webpages using
the Bento webpage segmentation service:

3. Petite Chez Scheme
4. Python

# Installation steps

0. Install the above dependencies.

1. Clone the repository to <path> of your choice. Add <path>/scheme to your IKARUS_LIBRARY_PATH  or VICARE_LIBRARY_PATH environment variable, depending on your usage of Ikarus or Vicare, respectively.

2. Go to <path>/pyxml2prog directory. Issue
    
        sh install.sh

# How to use

## Program induction example

To get started, run/read
    
    ikarus --script examples/basic-program-merge/sg-learn.ss

to get an idea of what it does. The basic structure:

    (import (beam-learning))

    (define test-data (list <example1> <example2> ... ))

    (define learned-program (learn-model test-data <beam-width> <depth-counter>)

## Grammar induction (Python interface)

Contained in bpm/examples/grammar-induction/grammar-induction.py:

    python grammar-induction.py <name-of-model> <model-scale> <beam-width> <likelihood-weight> <prior-weight> <dirichlet-parameter-alpha> <num-threads>

## Python/Bento interface for webpage layout analysis

There are three ways to induce programs from webpages: remotely pulling them from a file containing a list of pages, and using Bento to convert to XML, doing the same to a single remote site, or analyzing a single XML representation of a webpage:

    from pyxml2prog import *

    def multiple_test():
        run_multiple_from_pagelist("ex_pagelist.txt")

    def single_remote_test():
        page2prog_remote("http://www.google.com", "remote.ss")

    def single_local_test():
        page2prog_remote("ex_local.xml", "local.ss")
        
