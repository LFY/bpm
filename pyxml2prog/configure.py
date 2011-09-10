import os
from subprocess import Popen, PIPE

def run_cmd(cmd):
    p = Popen(cmd, shell=True, stdout=PIPE, stderr=PIPE)
    stdout, stderr = p.communicate()
    return stdout, stderr

cwd = os.getcwd()

def cmd_check(cmd):
    stdout, stderr = run_cmd(cmd)
    if "command not found" in stderr:
        return False
    else:
        return True
     
scheme_exe = 'ikarus'

if cmd_check('ikarus --script'):
    scheme_exe = 'ikarus'
elif cmd_check('vicare --script'):
    scheme_exe = 'vicare'
else:
    print "Ikarus/Vicare scheme not found."

if not cmd_check('petite --script'):
    print "Petite Chez Scheme not found."

configuration_template = """
scheme_exe = "%s"
this_location = "%s"
src_location = this_location + "pyxml2prog/"
ssax_dir = src_location + "SSAX/lib/"
""" %(scheme_exe, cwd + "/")

print >>open("%s/pyxml2prog/configuration.py" % cwd, "w"), configuration_template



