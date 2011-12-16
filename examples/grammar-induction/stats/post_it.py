import csv
import os
import glob
import sys

fileName = sys.argv[1]

def rowEntry (fileName):
    logFile = open(fileName)
    fileData = fileName.split('_')
   
    iteration = 0
    posterior = 0

    for line in logFile:
        lineList = line.split(' ') 
        if len(lineList)>0 and lineList[0]=="posterior":
            posterior=lineList[1]
            csvWriter.writerow([iteration, posterior])
            iteration = iteration + 1

if fileName == ".":
    for infile in glob.glob(os.path.join(fileName, '*.log')):
        print infile
        csvWriter = csv.writer(open(infile.replace("log","")+"post_it.csv", 'w'), delimiter=',')
        # what params should get written out?
        csvWriter.writerow(["iteration", "posterior"])
        rowEntry(infile)
else:
    csvWriter = csv.writer(open(fileName.replace("log","")+"post_it.csv", 'w'), delimiter=',')
    # what params should get written out?
    csvWriter.writerow(["iteration", "posterior"])
    rowEntry(fileName)