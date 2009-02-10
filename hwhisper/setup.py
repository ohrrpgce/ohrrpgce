# setup.py
from distutils.core import setup
import py2exe
import glob
import sys

import version

sys.argv[1:] = ["py2exe"]

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
