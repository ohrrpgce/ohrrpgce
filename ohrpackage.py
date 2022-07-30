#!/usr/bin/env python3
# Supports Python 2.x and 3.x

from __future__ import print_function
import sys
import os
import subprocess
import time
import shutil
import glob
if sys.version_info.major == 2:
    from ConfigParser import ConfigParser
else:
    from configparser import ConfigParser

import ohrbuild

host_win32 = sys.platform.startswith('win')

############################################################################
## Utilities

class temp_chdir():
    "Context manager to change working directory. Equivalent to contextlib.chdir() in Python 3.11"
    def __init__(self, directory):
        self.newdir = directory
    def __enter__(self):
        self.olddir = os.path.abspath(".")
        os.chdir(self.newdir)
    def __exit__(self, *dummy):
        os.chdir(self.olddir)


def copy_file_or_dir(src, dest):
    """Copy src to dest. If src is a directory acts recursively,
    while preserving any existing files in dest.
    Because shutil.copytree fails if a directory already exists.
    """
    # Create directory
    pardir = os.path.dirname(dest)
    if pardir:
        quiet_mkdir(pardir)

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
    assert len(path) > 3
    if os.path.isfile(path):
        if dry_run:
            print("rm", path)
        else:
            os.remove(path)

def safe_rmtree(path, dry_run = False):
    assert len(path) > 3
    if os.path.isdir(path):
        if dry_run:
            print("rmtree", path)
        else:
            shutil.rmtree(path)

def quiet_mkdir(dir):
    try:
        os.makedirs(dir)
    except OSError:
        pass # ignore dir-already-exists

def check_call(*args, **kwargs):
    print(" ".join(args))
    subprocess.check_call(args, **kwargs)

def relump(lumpdir, rpgfile):
    try:
        os.remove(rpgfile)
    except(OSError):
        # don't care if the file does not already exist
        pass
    os.system('relump "' + lumpdir + '" "' + rpgfile + '"')

def archive_dir(directory, outfile):
    """Produce an archive from a directory. Archived paths start with the directory path."""
    if outfile.endswith(".tar.bz2"):
        check_call("tar", "-jcf", outfile, directory)
    else:
        print("Unsupported archive filetype")
        sys.exit(1)

def archive_dir_contents(directory, outfile):
    """Produce an archive from a directory. Archived paths don't include the directory path."""
    outfile = os.path.abspath(outfile)
    with temp_chdir(directory):
        if outfile.endswith(".zip"):
            check_call("zip", "-q", outfile, *glob.glob("*"))
        else:
            print("Unsupported archive filetype")
            sys.exit(1)

############################################################################

def generate_buildinfo(game):
    "Run ohrrpgce-game/game.exe -buildinfo, using wine if needed, to generate buildinfo.ini next to it"
    directory, game = os.path.split(game)
    with temp_chdir(directory):
        safe_rm("buildinfo.ini")
        if game.endswith(".exe"):
            if host_win32:
                check_call(game, "-buildinfo", "buildinfo.ini")
            else:
                # env = dict(os.environ)
                # env["WINEDEBUG"] = "fixme-all"
                # Send stderr to a pipe to silence it
                check_call("wine", game, "-buildinfo", "buildinfo.ini", stderr = subprocess.PIPE)
        else:
            check_call("./" + game, "-buildinfo", "buildinfo.ini")
        assert os.path.isfile("buildinfo.ini")

def parse_buildinfo(path):
    "Returns a dict with buildinfo.ini contents"
    config = ConfigParser()
    config.read(path)
    return dict(config.items("buildinfo"))

def get_buildinfo(game):
    "Generate buildinfo.ini by running Game, and parse it"
    # If buildinfo.ini already exists next to Game, we overwrite it, otherwise
    # we delete it after we're done
    inifile = os.path.join(os.path.dirname(game), "buildinfo.ini")
    preexisting = os.path.isfile(inifile)
    generate_buildinfo(game)
    ret = parse_buildinfo(inifile)
    if not preexisting:
        safe_rm(inifile)
    return ret

############################################################################

class PackageContents():
    """Lists of files and directories to be packaged/installed.
    Each file path will be relative to srcdir.
    """
    def __init__(self, srcdir):
        self.srcdir = os.path.abspath(srcdir)
        self.datafiles = []
        self.executables = []
        self.icons = []

    def abspath(self, path):
        return os.path.join(self.srcdir, path)

    def glob(self, *patterns):
        "List of files matching any of several shell glob patterns, relative to self.srcdir"
        # glob.glob() gains a root_dir arg in Python 3.10
        with temp_chdir(self.srcdir):
            return sum(map(glob.glob, patterns), [])


def player_only_files(target, srcdir = ''):
    """Files (returned as a PackageContents) for a player-only package"""
    files = PackageContents(srcdir)

    if target == "win":
        files.executables = ["game.exe"]
    elif target == "mac":
        files.executables = ["OHRRPGCE-Game.app"]
    else:  #target == "linux":
        files.executables = ["ohrrpgce-game"]

    # Note: buildinfo.ini excluded because it's generated by archive_player
    files.datafiles = [
        "README-player-only.txt",
        "LICENSE-binary.txt"
    ]

    return files

def needed_windows_libs(buildinfo):
    "List of the Windows libraries needed by all compiled-in backends"
    gfxlibs = {
        "fb":      [],
        "sdl":     ["SDL.dll"],
        "sdl2":    ["SDL2.dll"],
        "directx": ["gfx_directx.dll"],
        "directx": ["gfx_directx.dll"],
    }
    musiclibs = {
        "silence": [],
        "native":  ["audiere.dll"],
        "native2": ["audiere.dll"],
        "sdl":     ["SDL.dll", "SDL_mixer.dll"],
        "sdl2":    ["SDL2.dll", "SDL2_mixer.dll"],
    }

    libs = set()
    for gfx in buildinfo['gfx'].split():
        libs.update(gfxlibs[gfx])
    for music in buildinfo['music'].split():
        libs.update(musiclibs[music])
    return list(libs)

def engine_files(target, config, srcdir = ''):
    """Files (returned as as PackageContents) for a Game+Custom package or for installation.
    Searches in srcdir, which should be the root of the source repo.
    config should be one of 'full', 'minimal' or 'nightly'.
    """
    exe = ".exe" if target == "win" else ""

    files = PackageContents(srcdir)

    # Files installed under $prefix/share/games/ohrrpgce on Unix,
    # installed in program directory on Windows and Mac
    files.datafiles = [
        "README-game.txt",
        "README-custom.txt",
        "IMPORTANT-nightly.txt",
        "LICENSE.txt",
        "LICENSE-binary.txt",
        "whatsnew.txt",
        "plotscr.hsd",
        "scancode.hsi",
        "unlump" + exe,
        "relump" + exe,
        "data",
        "ohrhelp",
        "docs/plotdict.xml",
        "docs/htmlplot.xsl",
        "docs/more-docs.txt",
    ] + files.glob(
        "docs/*.html",
        "docs/*.png",
    )

    if config == "full":
        files.datafiles += ["import"]

    # Files installed under $prefix/games on Unix,
    # installed in program directory on Windows and Mac
    # Includes libraries.
    if target == "win":
        files.executables += [
            "game.exe",
            "custom.exe",
            "hspeak.exe",
        ]
        buildinfo = get_buildinfo(files.abspath("game.exe"))
        files.executables += needed_windows_libs(buildinfo)
    elif target == "mac":
        files.executables += [
            "OHRRPGCE-Game.app",
            "OHRRPGCE-Custom.app",
        ]
    else:  #target == "linux":
        files.executables += [
            "ohrrpgce-game",
            "ohrrpgce-custom",
            "hspeak",
        ]

    # Files installed under $prefix/share/icons/... on Unix,
    # not used in Linux tarballs nor on Windows or Mac
    files.icons = [
        "ohrrpgce-game.png",
        "ohrrpgce-custom.png"
    ]

    return files


############################################################################

def gather_files(files):
    "Copy files (except icons) into a new directory named 'ohrrpgce' ready for archiving"

    # Place files in tmp/ in case we error out partway
    destdir = "tmp/"
    safe_rmtree(destdir)
    quiet_mkdir(destdir)

    for path in files.executables + files.datafiles:
        copy_file_or_dir(files.abspath(path), destdir + path)

    safe_rmtree("ohrrpgce")
    os.rename(destdir, "ohrrpgce")

def prepare_player(files):
    "Produces a player-only archive from files that were copied to the 'ohrrpgce' directory"
    with temp_chdir("ohrrpgce"):
        game = files.executables[0]
        check_call("strip", game)
        generate_buildinfo(game)
        files.datafiles += ["buildinfo.ini"]

def format_output_filename(template, srcdir = ''):
    "Expand replacements"
    today = time.strftime('%Y-%m-%d')
    codename, branch_name, branch_rev = ohrbuild.read_codename_and_branch(srcdir)
    return template.format(TODAY = today, CODENAME = codename, BRANCH = branch_name)

def package(target, config, outfile = None):
    if outfile:
        outfile = format_output_filename(outfile)

    if config == "player":
        files = player_only_files(target)
    else:
        files = engine_files(target, config)

    print("Gathering ohrrpgce/")
    gather_files(files)
    if config == "player":
        prepare_player(files)

    if outfile:
        print("Archiving " + outfile)
        if config == "player":
            archive_dir_contents("ohrrpgce", outfile)
        else:
            archive_dir("ohrrpgce", outfile)
        safe_rmtree("ohrrpgce")

############################################################################


if __name__ == '__main__':
    usage = """
Package the OHRRPGCE for a given OS and configuration. (Not all supported yet!)
Run after compiling all needed executables, from the root of the source tree.
Usage:
  ./ohrpackage.py {linux,win,mac} {full,minimal,nightly,player} [output_file]

If output_file is omitted files are instead placed in a directory named 'ohrrpgce'.
output_file can contain replacements {TODAY}, {CODENAME}, {BRANCH}."""

    outfile = None
    if len(sys.argv) == 4:
        outfile = sys.argv[3]
    elif len(sys.argv) != 3:
        print(usage)
        sys.exit(1)

    target = sys.argv[1]
    if target not in ("linux", "win"):
        print("Unsupported OS '%s'" % target)
        sys.exit(1)

    config = sys.argv[2]
    if config not in ("full", "minimal", "player"):
        print("Unsupported configuration '%s'" % config)
        sys.exit(1)

    package(target, config, outfile)
