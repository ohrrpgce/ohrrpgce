# setup.py
from distutils.core import setup
import py2exe
import os
import sys
import shutil
import re

import version

sys.argv[1:] = ["py2exe"]

gtk_dir = "C:\Program Files\GTK2-Runtime"

opts = {
    "py2exe": {
        "packages": "encodings",
        "includes": "pango,atk,gobject,cairo,pangocairo",
        }
    }

setup(
    name = "hwhisper",
    description = version.description,
    version = version.version,
    windows = [
        {"script": "hwhisper.py",
        "icon_resources": [(1, "hwhisper.ico")]
        }
    ],
    options=opts,
    data_files=[("hwhisper.xml")],
)

if not os.path.isdir("dist"): raise Exception("dist dir is missing")

print "Copying GTK+ support files.."
for dir in ["etc", "lib", "share"]:
    shutil.rmtree(os.path.join("dist", dir), True)
    shutil.copytree(os.path.join(gtk_dir, dir), os.path.join("dist", dir))

if not os.path.isdir("dist"): raise Exception("dist dir is missing")

print "Trimming non-default themes..."
keep = re.compile("^(Default|MS-Windows)", re.I)
dir = os.path.join("dist", "share", "themes")
for subdir in os.listdir(dir):
    match = keep.search(subdir)
    if match is None:
        shutil.rmtree(os.path.join(dir, subdir))
