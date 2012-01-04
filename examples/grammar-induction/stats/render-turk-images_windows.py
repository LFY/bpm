import glob
import os
import subprocess
import sys
import re 
gi_base_dir = os.getenv("GI_BASE_DIR")
print(gi_base_dir)

# need to add GI_BASE_DIR environ variable
# args are category, grammarType, & optionally to create maya scene files (0) or not (1)
# grammarType: lgcg, mgcg, bayes_100_10_1.0_1.0
category = sys.argv[1]
grammarType = sys.argv[2]
mode="0"
if(len(sys.argv)>3):
     mode = sys.argv[3]

globDir = os.path.join(gi_base_dir,"mturk","colladaMaya",category)

rigFile = os.path.join(globDir,"renderRig.ma")
rigFile = rigFile.replace("\\","/")

maya_script_dir = os.getenv("MAYA_SCRIPT_DIR")
print(maya_script_dir)

#load collada files into maya scenes
if(mode=="0"):
    scriptDir = os.path.join(gi_base_dir,"stats")
    os.putenv("MAYA_SCRIPT_PATH", scriptDir)

    melOutName = "collada2maya.mel"
    melOut = open(melOutName, 'w')
    melString = ""
    
    for infile in glob.glob(os.path.join(globDir, "*"+grammarType+"*.dae")):
        infile = infile.replace("\\","/")
        melString+="file -o \""+rigFile+"\"\n;file -i \""+ infile +"\";\nfile -rename \""+infile.replace(".dae",".ma")+"\";\nfile -save -type \"mayaAscii\";\nfile -f -new;\n"

    print(melString)

    melOut.write(melString) 
    melOut.close();

    subprocess.call(["mayabatch","-batch", "-command", "source " + melOutName + ""])

#render code
for infile in glob.glob(os.path.join(globDir, "*"+grammarType+"*.ma")):
    outName = infile.split('/')[-1].replace(".ma","")
   
    dirName = os.path.join(gi_base_dir,"mturk","images",category,grammarType)
    print(dirName)
    if not os.path.exists(dirName):
        os.makedirs(dirName)

    subprocess.call(["render","-r","sw","-rd",dirName,"-im",outName,"-of","png","-x","500","-y","500","-eaa","0",infile])
