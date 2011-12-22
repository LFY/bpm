import glob
import os
import subprocess

globDir = "/Users/ranju/projects/bpm/examples/grammar-induction/mturk/fodder/playground/exemplars/"

rigFile = "/Users/ranju/projects/bpm/examples/grammar-induction/mturk/fodder/playground/renderRig.ma"

melOutName = "/Applications/Autodesk/maya2012/Maya.app/Contents/scripts/myScripts/collada2maya.mel"
melOut = open(melOutName, 'w')
melString = ""

#load collada files into maya scenes
#if .ma files already exist don't bother recreating them
if (len(glob.glob(os.path.join(globDir, '*.ma')))==0):
    for infile in glob.glob(os.path.join(globDir, '*.dae')):
        melString+="file -o \""+rigFile+"\"\n;file -i \""+ infile +"\";\nfile -rename \""+infile.replace(".dae",".ma")+"\";\nfile -save -type \"mayaAscii\";\nfile -f -new;\n"

    melOut.write(melString) 
    melOut.close();

    subprocess.call(["maya","-batch", "-command", "source \"" + melOutName + "\""])

#render code
for infile in glob.glob(os.path.join(globDir, '*.ma')):
    outName = infile.split('/')[-1].replace(".ma","")
    dirName = globDir.replace("fodder","images")
    subprocess.call(["render","-r","sw","-rd",dirName,"-im",outName,"-of","png","-x","500","-y","500","-eaa","0",infile])