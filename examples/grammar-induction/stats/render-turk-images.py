import glob
import os
import subprocess
import sys

grammarDir = sys.argv[1]
flags=""
if (len(sys.argv)>2):
    flags = sys.argv[2]

enumDir = os.path.join(grammarDir,"enumeration")
mturkDir = os.path.join(grammarDir,"mturk-"+os.path.basename(grammarDir))
if not os.path.exists(mturkDir):
    os.makedirs(mturkDir)

if (len(glob.glob(os.path.join(enumDir, "*.dae")))==0 or (flags=="-c" or flags=="-cm")):
    subprocess.call(["generate-all-collada",grammarDir])

# load collada files into maya scenes
if (len(glob.glob(os.path.join(mturkDir, "*.mb")))==0 or (flags=="-m" or flags=="-cm")):
    rigFile = os.path.join(grammarDir,"..","renderStage.mb")
    rigFile = rigFile.replace("\\","/")
    
    melOutName = os.path.join(mturkDir, "collada-to-maya.mel")
    melOut = open(melOutName, 'w')
    melString = ""

    for infile in glob.glob(os.path.join(enumDir, "*.dae")):
        infile = infile.replace("\\","/")
        outfile = os.path.join(mturkDir,os.path.basename(infile.replace("dae","mb")))
        outfile.replace("\\","/")
        melString+="file -o \""+rigFile+"\";\nfile -i -gr -gn \"bldg_group\" \""+ infile +"\";\nfile -rename \""+outfile+"\";\nselect bldg_group;\nmove -y 42.0;\nfile -save -type \"mayaBinary\";\nfile -f -new;\n"

    melOut.write(melString)
    melOut.close()
        
    subprocess.call(["maya","-batch", "-command", "source \"" + melOutName + "\""])

#render code
for infile in glob.glob(os.path.join(mturkDir, "*.mb")):
    outName = os.path.basename(infile).replace(".mb","")
 
    subprocess.call(["render","-r","vray","-rd",mturkDir,"-im",outName,"-of","png","-x","500","-y","500","-cam","camera2",infile])