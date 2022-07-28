
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
    else:
        quiet_mkdir(dest)
        for filename in os.listdir(src):
            if filename == ".svn":
                continue
            srcpath = os.path.join(src, filename)
            destpath = os.path.join(dest, filename)
            copy_file_or_dir(srcpath, destpath)

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
    os.system(rootdir + '/relump "' + lumpdir + '" "' + rpgfile + '"')

############################################################################


if os.path.isfile('ohrrpgce-game'):
    prefix = ""
elif os.path.isfile('../ohrrpgce-game'):
    prefix = "../"
else:
    raise Exception("Can't find ohrrpgce-game and other files")

files = [
  prefix + "README-game.txt",
  prefix + "README-custom.txt",
  prefix + "LICENSE.txt",
  prefix + "LICENSE-binary.txt",
  prefix + "whatsnew.txt",
  prefix + "plotscr.hsd",
  prefix + "scancode.hsi",
  prefix + "unlump",
  prefix + "relump",
  prefix + "data",
  prefix + "ohrhelp"]

executables = [prefix + "ohrrpgce-game",
               prefix + "ohrrpgce-custom",
               prefix + "hspeak"]

icons = [prefix + "ohrrpgce-game.png",
         prefix + "ohrrpgce-custom.png"]
