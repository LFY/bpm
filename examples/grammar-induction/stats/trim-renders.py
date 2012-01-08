import glob
import os
import subprocess
import sys

dir = sys.argv[1]

for infile in glob.glob(os.path.join(dir,"*.png")):   
    subprocess.call(["convert",infile,"-bordercolor","white","-border","1x1","-fuzz","11%","-trim","+repage",os.path.basename(infile).replace(".png","")+"_trim.png"])