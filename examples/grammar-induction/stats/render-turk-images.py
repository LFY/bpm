import glob
import os
import subprocess
import sys

# need to add GI_BASE_DIR environ variable
# args are category, grammarType, & optionally to create maya scene files (0) or not (1)
# grammarType: lgcg, mgcg, bayes_100_10_1.0_1.0
category = sys.argv[1]
grammarType = sys.argv[2]
mode="0"
if(len(sys.argv)>3):
     mode = sys.argv[3]

globDir = os.environ.get("GI_BASE_DIR")+"mturk/colladaMaya/"+category

rigFile = globDir+"/renderRig.ma"


#load collada files into maya scenes
if(mode=="0"):
    scriptDir = os.environ.get("GI_BASE_DIR") + "stats"
    os.putenv("MAYA_SCRIPT_PATH", scriptDir)
    
    melOutName = scriptDir + "/collada2maya.mel"
    melOut = open(melOutName, 'w')  
    melString = ""
    
    for infile in glob.glob(os.path.join(globDir, "*"+grammarType+"*.dae")):
        melString+="file -o \""+rigFile+"\"\n;file -i \""+ infile +"\";\nfile -rename \""+infile.replace(".dae",".ma")+"\";\nfile -save -type \"mayaAscii\";\nfile -f -new;\n"

    melOut.write(melString) 
    melOut.close();

    subprocess.call(["maya","-batch", "-command", "source \"" + melOutName + "\""])

#render code
for infile in glob.glob(os.path.join(globDir, "*"+grammarType+"*.ma")):
    outName = infile.split('/')[-1].replace(".ma","")
    
    dirName = os.environ.get("GI_BASE_DIR")+"mturk/images/"+category+"/"+grammarType
    print dirName
    if not os.path.exists(dirName):
        os.makedirs(dirName)

    subprocess.call(["render","-r","sw","-rd",dirName,"-im",outName,"-of","png","-x","500","-y","500","-eaa","0",infile])