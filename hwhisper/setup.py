# setup.py
from distutils.core import setup
import py2exe
import glob

# This hasn't really been tested much yet.
# I mostly borrowed the "opts" from another pygtk project.

opts = {
    "py2exe": {
        "includes": "pango,atk,gobject,cairo,pangocairo",
        "dll_excludes": [
        "iconv.dll","intl.dll","libatk-1.0-0.dll",
        "libgdk_pixbuf-2.0-0.dll","libgdk-win32-2.0-0.dll",
        "libglib-2.0-0.dll","libgmodule-2.0-0.dll",
        "libgobject-2.0-0.dll","libgthread-2.0-0.dll",
        "libgtk-win32-2.0-0.dll","libpango-1.0-0.dll",
        "libpangowin32-1.0-0.dll"],
        }
    }

setup(
    name = "hwhisper",
    description = "Hamster Whisper source code editor for OHRRPGCE PlotScripting",
    version = "0.1",
    windows = [
        {"script": "hwhisper.py",
        "icon_resources": [(1, "hwhisper.ico")]
        }
    ],
    options=opts,
    data_files=[("hwhisper.xml")],
)
