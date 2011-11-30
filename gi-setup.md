# Grammar Induction Setup

## Prereqs and where to download:


1. SWI-Prolog:

    http://www.swi-prolog.org/download/stable

2. Ikarus (or Vicare) Scheme:

    http://dl.dropbox.com/u/3153486/ikarus.dev.tgz

This builds with a ./configure; make; make install workflow.

An alternative is to try a more up-to-date, renamed version of Ikarus:

    https://github.com/downloads/marcomaggi/vicare/vicare-0.2d1-1.tar.gz

which follows a similar setup process.

This also requires libffi and gmp; you'll need to add the proper -I and -L flags
to the ./configure step if it does not detect them automatically. 

See 

    http://projects.csail.mit.edu/church/wiki/Installing_Bher

for an example of what flags to supply to configure.  I am not sure
if this works for Cygwin, though.

3. Petite Chez Scheme:

http://www.scheme.com/download/

4. Python 2.7 is also required.

After installing these, make sure the binaries
    
    swipl
    ikarus (or vicare)
    petite

are all accessible through the command shell from which you launch Python.

## After installing prereqs:

Install the grammar induction library by issuing

    git clone git@github.com:LNFY/bpm.git

and then going to the directory where it unpacks, and issuing

    cd pyxml2prog; sh install.sh .

This installs a Python library for doing the grammar induction. It may require admin privileges.

# Usage:

The main interface is the script 

    grammar-induction.py

in this part of the repository:

    examples/grammar-induction.

The interface is

    python grammar-induction.py <filename-of-model> <model-scale> <beam-width> <likelihood-weight> <prior-weight> <dirichlet-parameter-alpha> <num-threads>

