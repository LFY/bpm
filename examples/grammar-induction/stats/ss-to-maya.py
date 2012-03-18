import glob
import os
import subprocess
import sys

pTestDir = sys.argv[1]

melOutName = os.path.join(pTestDir, "ss-to-maya.mel")
    
# runs mel file
subprocess.call(["maya","-batch", "-command", "source \"" + melOutName + "\""])
