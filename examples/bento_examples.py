from pyxml2prog import *

def multiple_test():
    run_multiple_from_pagelist("ex_pagelist.txt")

def single_remote_test():
    page2prog_remote("http://www.google.com", "remote.ss")

def single_local_test():
    page2prog_remote("ex_local.xml", "local.ss")
