import subprocess
import sys
import os
import glob
import shutil

grammarDir = sys.argv[1]
grammarFile = glob.glob(os.path.join(grammarDir,"*.grammar.ss"))[0]

spacing = "50.0"
if(len(sys.argv)>2):
    spacing=sys.argv[2]

mode="1"
if(len(sys.argv)>3):
    mode = sys.argv[3]

modeArg="200"
if(mode=="0"):
    modeArg="0.01"
if(mode=="2"):
    modeArg="0.9"
if(len(sys.argv)>4):
    modeArg=sys.argv[4]

subprocess.call(["vicare","--script","enumerate-derivations.ss", grammarFile, spacing, mode, modeArg])

# completely deletes what was already in enumeration
writeDir = os.path.join(grammarDir, "enumeration")
if os.path.exists(writeDir):
    shutil.rmtree(writeDir)
os.makedirs(writeDir)
writeString = ""
for infile in glob.glob("d_*"):
    parts=infile.split("_")
    counter='%.05d'% int(parts[1])
    prob='%.2e'% float(parts[2])
    newFileName="d_"+counter+"_"+prob+".ss"
    shutil.move(infile, os.path.join(writeDir,newFileName))
    writeString+=counter+"\t"+prob+"\n"

enumerationSummary = open(os.path.join(writeDir, "enumeration-summary.txt"), 'w')
enumerationSummary.write(writeString)
enumerationSummary.close()

