import glob
import os
import subprocess

globDir = "/Users/ranju/projects/bpm/examples/grammar-induction/mturk/fodder/seuss/training/"
melOutName = "/Applications/Autodesk/maya2012/Maya.app/Contents/scripts/myScripts/collada2maya.mel"
melOut = open(melOutName, 'w')

melString = ""

# TODO: create rig.ma file
#if .ma files already exist don't both redoing the loading
if (len(glob.glob(os.path.join(globDir, '*.ma')))==0):
    for infile in glob.glob(os.path.join(globDir, '*.dae')):
        melString+="file -i \""+ infile +"\";\nfile -rename \""+infile.replace(".dae",".ma")+"\";\nfile -save -type \"mayaAscii\";\nfile -f -new;\n"

melOut.write(melString)

melOut.close();

subprocess.call(["maya","-batch", "-command", "source \"" + melOutName + "\""])

for infile in glob.glob(os.path.join(globDir, '*.ma')):
    outName = infile.split('/')[-1].replace(".ma","")
    dirName = globDir.replace("fodder","images")
    subprocess.call(["render","-r","mr","-rd",dirName,"-im",outName,"-of","png","-x","200","-y","200",infile])