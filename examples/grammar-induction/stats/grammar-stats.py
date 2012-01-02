import csv
import os
import glob
import sys

# TODO: finalize file name format
# TODO: add P(E)

def rowEntry (logDir):
    csvWriter = csv.writer(open(os.path.join(logDir,"grammar-stats-summary.csv"), 'w'), delimiter=',')
    csvWriter.writerow(["grammar_type", "beam_width", "likelihood_weight/prior_weight", "prior_parameter", "log L", "MDL", "posterior"])
    
    logFileName = glob.glob(os.path.join(logDir,"*.log"))[0]
    logFile = open(logFileName)
    print (os.path.basename(logFileName))
    fileData = os.path.basename(logFileName).split('_')
    
    likelihood=0
    descLength=0
    posterior=0
    
    for line in logFile:
        lineList = line.split(' ') 
        if len(lineList)>0 and lineList[0]=="likelihood+weight":
            likelihood=lineList[1]
        if len(lineList)>0 and lineList[0]=="desc-length":
            descLength=lineList[1]
        if len(lineList)>0 and lineList[0]=="posterior":
            posterior=lineList[1]

        
    csvWriter.writerow([fileData[2], fileData[4], float(fileData[5])/(1.0*float(fileData[6])), fileData[7], likelihood, descLength, posterior])

if (len(sys.argv)<3):
    dir = sys.argv[1]
    rowEntry(dir)

elif (len(sys.argv)>2 and (sys.argv[2]=="-a" or sys.argv[2]=="-ar")):
    dir = sys.argv[1]
    
    allWriter = csv.writer(open(os.path.join(dir,"all-stats-summary.csv"), 'w'), delimiter=',')
    allWriter.writerow(["grammar_type", "beam_width", "likelihood_weight/prior_weight", "prior_parameter", "log L", "MDL", "posterior"])

    for subDir in glob.glob(os.path.join(dir,"*")):
        if(len(glob.glob(os.path.join(subDir,"*.log")))>0):
            statsFile = os.path.join(subDir,"grammar-stats-summary.csv")
            if((not os.path.exists(statsFile)) or sys.argv[2]=="-ar"):
                rowEntry(subDir)
            s = open(statsFile)
            s.next()
            for line in s:
                allWriter.writerow(line.split(","))
else:
    print("not valid input")

# TDOO: pretty-print table


