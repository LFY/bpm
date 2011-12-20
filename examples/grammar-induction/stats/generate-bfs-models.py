import subprocess
import sys
import os
import glob

grammarFile = sys.argv[1]
probabilityThreshold="0.01"
if(len(sys.argv)>2):
    probabilityThreshold = sys.argv[2]
mode="0"
if(len(sys.argv)>3):
    mode = sys.argv[3]

fileData = grammarFile.split('_')

subprocess.call(["vicare","--script","../stats/generate-bfs-models.ss",grammarFile, probabilityThreshold, mode])

for infile in glob.glob("bfs.dae.scene*"):
    subprocess.call(["python","../reconst-dae.py",fileData[0]+"_"+fileData[1], infile, grammarFile.replace(".dae","").replace(".grammar.ss",".bfs")+infile[-1]+".dae"])
    subprocess.call(["rm", infile])
