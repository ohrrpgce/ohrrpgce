
from __future__ import print_function
import sys
import os
import shutil

import ohrbuild

############################################################################
## Utilities

def copy_file_or_dir(src, dest):
    """Copy src to dest. If src is a directory acts recursively,
    while preserving any existing files in dest.
    Because shutil.copytree fails if a directory already exists.
    """
    if os.path.isfile(src):
        shutil.copy2(src, dest)
    elif os.path.isdir(src):
        quiet_mkdir(dest)
        for filename in os.listdir(src):
            if filename == ".svn":
                continue
            srcpath = os.path.join(src, filename)
            destpath = os.path.join(dest, filename)
            copy_file_or_dir(srcpath, destpath)
    else:
        raise Exception("Missing " + src)

def safe_rm(path, dry_run = False):
    assert len(path) > 5
    if os.path.isfile(path):
        print("rm", path)
        if not dry_run:
            os.remove(path)

def safe_rmtree(path, dry_run = False):
    assert len(path) > 5
    if os.path.isdir(path):
        print("rmtree", path)
        if not dry_run:
            shutil.rmtree(path)

def quiet_mkdir(dir):
    try:
        os.makedirs(dir)
    except OSError:
        pass # ignore dir-already-exists

def relump(lumpdir, rpgfile):
    try:
        os.remove(rpgfile)
    except(OSError):
        # don't care if the file does not already exist
        pass
    os.system('relump "' + lumpdir + '" "' + rpgfile + '"')

############################################################################


class PackageContents():
    def __init__(self, srcdir):
        self.srcdir = os.path.abspath(srcdir)
        self.datafiles = []
        self.executables = []
        self.icons = []

    def abspath(self, path):
        return os.path.join(self.srcdir, path)


def files_to_package(srcdir = '', win32 = False):
    """Return lists of file and directory paths to be packaged/installed.
    Searches in srcdir which should be the root of the source repo.
    Each returned path will be either absolute or relative to srcdir.
    """

    exe = '.exe' if win32 else ''

    files = PackageContents(srcdir)

    # Files installed under $prefix/share/games/ohrrpgce on Unix,
    # installed in program directory on Windows and Mac
    files.datafiles = [
        "README-game.txt",
        "README-custom.txt",
        "LICENSE.txt",
        "LICENSE-binary.txt",
        "whatsnew.txt",
        "plotscr.hsd",
        "scancode.hsi",
        "unlump" + exe,
        "relump" + exe,
        "data",
        "ohrhelp"
    ]

    # Files installed under $prefix/games on Unix,
    # installed in program directory on Windows and Mac
    files.executables = [
        "hspeak" + exe
    ]
    if win32:
        files.executables += [
            "game.exe",
            "custom.exe",
        ]
    else:
        files.executables += [
            "ohrrpgce-game",
            "ohrrpgce-custom",
        ]

    # Files installed under $prefix/share/icons/... on Unix,
    # not used on Windows and Mac
    files.icons = [
        "ohrrpgce-game.png",
        "ohrrpgce-custom.png"
    ]

    return files
