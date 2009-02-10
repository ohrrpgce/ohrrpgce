# setup.py
from distutils.core import setup
import py2exe
import glob

import version

# This hasn't really been tested much yet.
# I mostly borrowed the "opts" from another pygtk project.

sys.argv[1:] = ["py2exe", "-O0", "-b", "1"]

opts = {
    "py2exe": {
        "includes": "pango,atk,gobject,cairo,pangocairo",
        }
    }

setup(
    name = "hwhisper",
    description = version.description,
    version = "0.1",
    windows = [
        {"script": "hwhisper.py",
        "icon_resources": [(1, "hwhisper.ico")]
        }
    ],
    options=opts,
    data_files=[("hwhisper.xml")],
)
