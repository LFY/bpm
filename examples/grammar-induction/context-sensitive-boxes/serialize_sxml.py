from pyxml2prog import *

import sys

print_sxml(sys.argv[1], sys.argv[1].rstrip("sxml") + "graffle")
