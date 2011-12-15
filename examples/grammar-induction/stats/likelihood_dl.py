import csv
import os
import glob
import sys

fileName = sys.argv[1]

def rowEntry (fileName):
    logFile = open(fileName)
    fileData = fileName.split('_')
    
    likelihood=0
    descLength=0
    for line in logFile:
        lineList = line.split(' ') 
        if len(lineList)>0 and lineList[0]=="likelihood+weight":
            likelihood=lineList[1]
        if len(lineList)>0 and lineList[0]=="desc-length":
            descLength=lineList[1]
        
    csvWriter.writerow([fileData[0].replace("./",""), fileData[2], fileData[3], fileData[4], fileData[5], fileData[6], fileData[7], likelihood, descLength])

if fileName == ".":
    csvWriter = csv.writer(open("likelihood_dl.csv", 'w'), delimiter=',')
    # what params should get written out?
    csvWriter.writerow(["category", "grammar_type", "model_scale", "beam_width", "likelihood_weight", "prior_weight", "dirichelet_alpha", "likelihood", "desc_length"])
    for infile in glob.glob(os.path.join(fileName, '*.log')):
        rowEntry(infile)
else:
    csvWriter = csv.writer(open(fileName.replace("log","")+"likelihood_dl.csv", 'w'), delimiter=',')
    # what params should get written out?
    csvWriter.writerow(["category", "grammar_type", "model_scale", "beam_width", "likelihood_weight", "prior_weight", "dirichelet_alpha", "likelihood", "desc_length"])
    rowEntry(fileName)


