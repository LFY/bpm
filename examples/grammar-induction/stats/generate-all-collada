import subprocess
import sys
import os
import glob

baseDir = sys.argv[1]
readWriteDir = os.path.join(baseDir, "enumeration", "*")

# once it is in PATH, get rid of this line
scriptFile = os.path.join(os.getenv("GI_BASE_DIR"),"bpm","examples","grammar-induction","reconst-dae.py")

for infile in glob.glob(readWriteDir):
    origDAE = os.path.join(os.path.join(baseDir,".."),"exemplars.dae")
    outfile = infile.replace(".ss",".dae")
    subprocess.call(["python",scriptFile,origDAE, infile, outfile])

