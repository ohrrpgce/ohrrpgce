#!/usr/bin/env python3
# Supports Python 2.x and 3.x

from __future__ import print_function
import sys
import os
import io
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

rootdir = os.path.abspath(os.path.dirname(__file__))

############################################################################
## Utilities

class PackageError(Exception): pass
class MissingFile(PackageError): pass


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
        raise MissingFile("Missing " + src)

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

def unix2dos(path):
    # Equivalent to open() in Python 3
    with io.open(path, "r", encoding = "latin1") as f:
        lines = f.readlines()
    temp = path + "__dos"
    with io.open(temp, "w", newline = "\r\n", encoding = "latin1") as f:
        f.writelines(lines)
    shutil.copystat(path, temp)
    os.remove(path)
    os.rename(temp, path)

def find_program(name):
    if host_win32:
        support_exe = os.path.join(rootdir, "support", name + ".exe")
        if os.path.isfile(support_exe):
            return support_exe
    return name

def clip_string(s, maxlen = 90):
    "Shorten a string if over maxlen"
    return s if len(s) <= maxlen else s[:maxlen-5] + "[...]"

def subprocess_args(prog, *args):
    return (find_program(prog), ) + args

def check_call(*args, **kwargs):
    subprocess.check_call(subprocess_args(*args), **kwargs)

def print_check_call(*args, **kwargs):
    args = subprocess_args(*args)
    print(clip_string(" ".join(args), 200))
    subprocess.check_call(args, **kwargs)

def check_output(*args, **kwargs):
    subprocess.check_output(subprocess_args(*args), **kwargs)

def check_wine_call(prog, *args):
    "Call wine as needed. Prefix prog with @ to silence stderr and wine spam"
    silent = prog[0] == "@"
    prog = prog.lstrip("@")
    if prog.endswith(".exe") and not host_win32:
        kwargs = {"env": dict(os.environ)}
        kwargs["env"]["WINEDEBUG"] = "fixme-all"
        if silent:
            # Send stderr to a pipe to silence remaining spam
            kwargs["stderr"] = subprocess.PIPE
        print_check_call("wine", prog, *args, **kwargs)
    else:
        print_check_call(prog, *args)

def relump_game(lumpdir, rpgfile):
    safe_rm(rpgfile)
    check_call("relump", lumpdir, rpgfile)

def get_ext(path):
    "Unlike os.path.splitext, splits from first . not last"
    path = os.path.basename(path)[1:]  # Trim any leading .
    if "." not in path:
        return ""
    return path[path.index(".") + 1 :]

def archive_dir(directory, outfile):
    """Produce an archive from a directory. Archived paths start with the directory path."""
    ext = get_ext(outfile)
    if ext not in "tar.bz2 zip 7z".split():
        raise PackageError("Unsupported archive filetype " + ext)
    safe_rm(outfile)
    if ext == "tar.bz2":
        check_call("tar", "-jcf", outfile, directory)
    elif ext == "zip":
        check_call("zip", "-9", "-q", "-r", outfile, directory)
    elif ext == "7z":
        # Capture stdout to silence it (errors printed to stderr)
        check_output("7za", "a", "-mx=7", "-bd", outfile, directory)

def archive_dir_contents(directory, outfile):
    """Produce an archive from a directory. Archived paths don't include the directory path."""
    ext = get_ext(outfile)
    if ext not in "zip 7z".split():
        raise PackageError("Unsupported archive filetype " + ext)
    safe_rm(outfile)
    outfile = os.path.abspath(outfile)
    with temp_chdir(directory):
        if ext == "zip":
            check_call("zip", "-9", "-q", "-r", outfile, *glob.glob("*"))
        elif ext == "7z":
            # Capture stdout to silence it (errors printed to stderr)
            check_output("7za", "a", "-mx=7", "-bd", outfile, *glob.glob("*"))

############################################################################

def generate_buildinfo(game):
    "Run ohrrpgce-game/game.exe -buildinfo, using wine if needed, to generate buildinfo.ini next to it"
    directory, game = os.path.split(game)
    with temp_chdir(directory):
        safe_rm("buildinfo.ini")
        check_wine_call("@./" + game, "-buildinfo", "buildinfo.ini")
        assert os.path.isfile("buildinfo.ini")

def parse_buildinfo(path):
    "Returns a dict with buildinfo.ini contents"
    config = ConfigParser()
    config.read(path)
    return dict(config.items("buildinfo"))

def get_buildinfo(game, keep_file = False):
    """Generate buildinfo.ini by running Game, and parse it.
    If buildinfo.ini already exists next to Game, overwrite it,
    otherwise delete it afterwards, unless keep_file."""
    inifile = os.path.join(os.path.dirname(game), "buildinfo.ini")
    preexisting = os.path.isfile(inifile)
    generate_buildinfo(game)
    ret = parse_buildinfo(inifile)
    if not preexisting and not keep_file:
        safe_rm(inifile)
    return ret

############################################################################

class PackageContents():
    """Lists of files and directories to be packaged/installed.
    Each file path will be relative to srcdir.
    If a file is an .rpg then gather_files() look for an .rpgdir and relump it.
    The destination directories for files can be overridden with setdest().
    """
    def __init__(self, srcdir):
        self.srcdir = os.path.abspath(srcdir)
        self.datafiles = []
        self.executables = []
        self.icons = []
        self.dest = {}

    def setdest(self, path, destdir):
        self.dest[path] = destdir
        return path

    def getdest(self, path):
        "Get the (relative) path at which to place a file"
        if path in self.dest:
            return os.path.join(self.dest[path], os.path.basename(path))
        return path

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

    game = files.abspath(files.executables[0])
    if not os.path.isfile(game):
        raise MissingFile("Missing " + game)

    buildinfo = get_buildinfo(game, keep_file = True)
    print("buildinfo: gfx=%s,  music=%s" % (buildinfo['gfx'], buildinfo['music']))
    if target == "win":
        files.executables += needed_windows_libs(buildinfo)

    files.datafiles = [
        "buildinfo.ini",
        "README-player-only.txt",
        "LICENSE-binary.txt"
    ]

    return files

def symbols_files(target, srcdir = ''):
    """Files (returned as a PackageContents) for a  package"""
    if target != "win":
        raise PackageError("Can only package Windows symbols")

    files = PackageContents(srcdir)
    files.datafiles = [
        "game.exe",
        "custom.exe",
        "win32/custom.pdb",
        "win32/game.pdb",
    ]
    files.dest["win32/custom.pdb"] = ""
    files.dest["win32/game.pdb"] = ""
    return files

def needed_windows_libs(buildinfo):
    "List of the Windows libraries needed by all compiled-in backends"
    gfxlibs = {
        "fb":      [],
        "sdl":     ["SDL.dll"],
        "sdl2":    ["SDL2.dll"],
        "directx": ["gfx_directx.dll"],
        "alleg":   ["alleg40.dll"],
    }
    musiclibs = {
        "silence": [],
        "native":  ["audiere.dll"],
        "native2": ["audiere.dll"],
        "sdl":     ["SDL.dll", "SDL_mixer.dll"],
        "sdl2":    ["SDL2.dll", "SDL2_mixer.dll"],
        "allegro": ["alleg40.dll"],
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

    # files.datafiles are installed under $prefix/share/games/ohrrpgce on Unix,
    # installed in program directory on Windows and Mac
    if config == "nightly":
        files.datafiles += ["README-nightly.txt"]
    files.datafiles += [
        "README-game.txt",
        "README-custom.txt",
        "IMPORTANT-nightly.txt",
        "LICENSE-binary.txt",
        "whatsnew.txt",
        "plotscr.hsd",
        "scancode.hsi",
        "data",
        "docs/plotdictionary.html",
        "docs/more-docs.txt",
    ] + files.glob(
        "docs/*.png",
        "ohrhelp/*.txt",
    )
    if target == "win":
        if config == "full":
            files.datafiles += files.glob(
                "docs/hamsterspeak.html",
                "docs/plotscripttutor.html",
                "docs/plotdict.xml",
                "docs/htmlplot.xsl",
                "docs/FAQ.URL",
                "docs/HOWTO.URL",
            )
        elif config == "minimal":
            files.datafiles += files.glob(
                "docs/*.URL",  # Unlike full, adds Plotdictionary.URL
            )
        else: # nightly
            pass
    else: # linux
        if config in ("full", "nightly"):  #linux-minimal config isn't used anyway
            files.datafiles += [
                "docs/hamsterspeak.html",
                "docs/plotscripttutor.html",
                "docs/plotdict.xml",
                "docs/htmlplot.xsl",
            ]

    if os.path.isfile(files.abspath("svninfo.txt")):
        # Created by distrib-nightly-win.bat. We could also generate it here. See ohrbuild.query_svn_rev_and_date()
        files.datafiles += ["svninfo.txt"]

    if config == "full":
        # NOTE: import/ is specially excluded from .deb and linux "scons install"
        files.datafiles += ["import"]

    # Support utilities
    if config == "full": # and installer
        files.datafiles += [
            "relump" + exe,
            "unlump" + exe,
        ]
    elif config == "nightly":
        files.datafiles += [
            "relump" + exe,
        ]
    if target == "win":
        # These aren't always included
        files.setdest("relump.exe", "support")
        files.setdest("unlump.exe", "support")

    if target == "win":
        files.datafiles += [
            "support/wget.exe",
        ]
        if config in ("full", "nightly"):
            files.datafiles += [
                "support/madplay.exe",
                "support/LICENSE-madplay.txt",
                "support/oggenc.exe",
                "support/LICENSE-oggenc.txt",
                "support/zip.exe",
                "support/zip-version.txt",
                "support/unzip.exe",
                "support/CrashRpt1403.dll",
                "support/CrashSender1403.exe",
                "support/crashrpt_lang.ini",
                "support/LICENSE-crashrpt.txt",
            ]
        if config == "full":
            files.datafiles += [
                "support/zip_exec.exe",
                "support/LICENSE-zip_exec.txt",
                "support/rcedit.exe",
                "support/rcedit-version.txt",
                "support/LICENSE-rcedit.txt",
            ]

    # files.executables are installed under $prefix/games on Unix,
    # installed in program directory on Windows and Mac
    # Includes libraries.
    if target == "win":
        files.executables += [
            "game.exe",
            "custom.exe",
            "hspeak.exe",
        ]
        buildinfo = get_buildinfo(files.abspath("game.exe"))
        print("buildinfo: gfx=%s,  music=%s" % (buildinfo['gfx'], buildinfo['music']))
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

def add_vikings_files(files):
    files.datafiles += [
        files.setdest("vikings/vikings.rpg", ""),  # Will be generated from vikings.rpgdir
        files.setdest("vikings/Vikings script files", ""),
        files.setdest("vikings/README-vikings.txt", "")
    ]

############################################################################

def gather_files(files, target, extrafiles = []):
    "Copy files (except icons) into a new directory named 'ohrrpgce' ready for archiving"

    # Place files in tmp/ in case we error out partway
    destdir = "tmp/"
    safe_rmtree(destdir)
    quiet_mkdir(destdir)

    for path in files.executables + files.datafiles + extrafiles:
        src = files.abspath(path)
        dest = destdir + files.getdest(path)
        # Automatically relump an .rpgdir to .rpg
        if path.endswith(".rpg") and os.path.isdir(src + "dir"):
            relump_game(src + "dir", dest)
        else:
            copy_file_or_dir(src, dest)

    if target == "win":
        print("Converting *.txt/hsi/hsd newlines")
        with temp_chdir("tmp"):
            for dirpath, dirs, files in os.walk('.'):
                for fname in files:
                    if os.path.splitext(fname)[1] in (".txt", ".hsi", ".hsd"):
                        unix2dos(os.path.join(dirpath, fname))

    safe_rmtree("ohrrpgce")
    os.rename(destdir, "ohrrpgce")

def prepare_player(files, target):
    "Produces a player-only archive from files that were copied to the 'ohrrpgce' directory"
    # game.exe already fully stripped if scons pdb=1 used, and in fact strip likely will fail
    if target != "win":
        with temp_chdir("ohrrpgce"):
            game = files.executables[0]
            check_call("strip", game)

def format_output_filename(template, srcdir = ''):
    "Expand replacements"
    today = time.strftime('%Y-%m-%d')
    codename, branch_name, branch_rev = ohrbuild.read_codename_and_branch(srcdir)
    return template.format(TODAY = today, CODENAME = codename, BRANCH = branch_name)

def package(target, config, outfile = None, extrafiles = []):
    if outfile:
        outfile = format_output_filename(outfile)

    if config == "player":
        files = player_only_files(target)
    elif config == "symbols":
        files = symbols_files(target)
    elif config == "full+vikings":
        files = engine_files(target, "full")
        add_vikings_files(files)
    else:
        files = engine_files(target, config)

    print("Gathering ohrrpgce/")
    gather_files(files, target, extrafiles)
    if config == "player":
        prepare_player(files, target)

    if outfile:
        print("Archiving " + outfile)
        if config in ("player", "symbols") or target == "win":
            archive_dir_contents("ohrrpgce", outfile)
        else:
            archive_dir("ohrrpgce", outfile)
        safe_rmtree("ohrrpgce")

############################################################################


if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(formatter_class = argparse.RawTextHelpFormatter, description =
"""Package the OHRRPGCE for a given OS and configuration. (Not all supported yet!)
Run after compiling all needed executables as desired, from the root of the source tree.

If outfile is omitted, files are instead placed in a directory named 'ohrrpgce'.""")

    if "--" in sys.argv:
        dashdash = sys.argv.index("--")
    else:
        dashdash = len(sys.argv)
    argv = sys.argv[1 : dashdash]
    extrafiles = sys.argv[dashdash + 1 :]

    parser.add_argument("target", choices = ("linux", "win", "mac"), help = "OS to package for (Windows from Unix requires wine)")
    parser.add_argument("config", metavar = "config", choices = ("full", "full+vikings", "nightly", "minimal", "player", "symbols"), help =
"""What to package:
full:         Complete OHRRPGCE package (except Vikings)
full+vikings: Adds Vikings of Midgard
nightly:      Slightly leaner, excludes import/ and some utilities
minimal:      Excludes import/, plotdict.xml and unnecessary support utilities
player:       Just Game, for distributing games
symbols:      Windows .pdb debug symbols, for process_crashrpt_report.py""")
    parser.add_argument("outfile", nargs = "?", help = 
"""Output file path. Should end in a supported archive format (e.g. .zip, .7z).
Can contain text replacements {TODAY}, {CODENAME}, {BRANCH}.""")
    # This argument is added just to add to the usage
    parser.add_argument("dummy", nargs="?",metavar = "-- file [file ...]", help = "Extra files to include")

    if not argv:
        parser.print_help()
        exit()

    args = parser.parse_args(argv)
    if args.dummy:
        parser.print_usage()
        exit("Unexpected arg after outfile: " + args.dummy)

    try:
        package(args.target, args.config, args.outfile, extrafiles)
    except (PackageError, subprocess.CalledProcessError) as e:
        exit("Error: " + str(e))
