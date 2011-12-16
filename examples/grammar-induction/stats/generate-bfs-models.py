import subprocess
import sys
import os
import glob

grammarFile = sys.argv[1]
fileData = grammarFile.split('_')

subprocess.call(["vicare","--script","../stats/generate-bfs-models.ss",grammarFile])

for infile in glob.glob("bfs.dae.scene*"):
    subprocess.call(["python","../reconst-dae.py",fileData[0]+"_"+fileData[1],infile, fileData[0]+"_"+infile.replace(".dae","")+".dae"])
    subprocess.call(["rm", infile])
