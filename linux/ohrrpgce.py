#!/usr/bin/env python3
# Supports Python 2.x and 3.x

"""
For installing/uninstalling the OHRRPGCE locally system-wide under Linux (implementation of "scons install"
and "scons uninstall", and creating the ohrrpgce .deb package (when invoked from the commandline).
"""

from __future__ import print_function
import sys
import os
import shutil

# Location of ohrbuild.py, codename.txt, etc
rootdir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
sys.path.append(rootdir)

import ohrbuild
from ohrpackage import engine_files, copy_file_or_dir, safe_rm, safe_rmtree, quiet_mkdir


############################################################################
## Installing and uninstalling system-wide on Linux

def build_tree(destdir, package_name, files, prefix = "/usr"):
    # Executables
    if len(files.executables):
        dest = destdir + prefix + "/games/"
        quiet_mkdir(dest)
        for exe in files.executables:
            shutil.copy2(files.abspath(exe), dest + exe)
    # Icons
    if len(files.icons):
        # Icons should be either .png or .svg.
        # My reading of the freedesktop.org standard is that it doesn't matter where you put the icon
        # if you only have one resolution.
        dest = destdir + prefix + "/share/icons/hicolor/32x32/apps/"
        quiet_mkdir(dest)
        for icon in files.icons:
            shutil.copy2(files.abspath(icon), dest + icon)
    # Data files
    dest = destdir + prefix + "/share/games/" + package_name + "/"
    quiet_mkdir(dest)
    for path in files.datafiles:
        copy_file_or_dir(files.abspath(path), dest + path)

def rm_tree(destdir, package_name, files, prefix = "/usr", dry_run = False):
    # Executables
    if len(files.executables):
        dest = destdir + prefix + "/games/"
        for exe in files.executables:
            safe_rm(dest + exe, dry_run)
    # Icons
    if len(files.icons):
        # Icons should be either .png or .svg.
        # My reading of the freedesktop.org standard is that it doesn't matter where you put the icon
        # if you only have one resolution.
        dest = destdir + prefix + "/share/icons/hicolor/32x32/apps/"
        for icon in files.icons:
            safe_rm(dest + icon, dry_run)
    # Data files
    dest = destdir + prefix + "/share/games/" + package_name + "/"
    safe_rmtree(dest, dry_run)

def menu_entry(destdir, package_name, title, command, append=False, desktop_file_suffix="", prefix = "/usr", icon = None):
    mode = "a" if append else "w"
    quiet_mkdir(destdir + prefix + "/share/menu/")
    f = open(destdir + prefix + "/share/menu/" + package_name, mode)
    s = '?package(%s): needs="X11" title="%s" command="%s" section="Games/RolePlaying"\n' % (package_name, title, command)
    f.write(s)
    f.close()
    quiet_mkdir(destdir + prefix + "/share/applications/")
    f = open(destdir + prefix + "/share/applications/" + package_name + desktop_file_suffix + ".desktop", "w")
    s = """[Desktop Entry]
Name=%s
Exec=%s
Terminal=false
Type=Application
Categories=Application;Game;
""" % (title, command)
    if icon:
        s += "Icon=%s\n" % (icon,)
    f.write(s)
    f.close()

def remove_menu_entry(destdir, package_name, desktop_file_suffix="", prefix = "/usr", dry_run = False):
    path = destdir + prefix + "/share/menu/" + package_name
    safe_rm(path, dry_run)
    path = destdir + prefix + "/share/applications/" + package_name + desktop_file_suffix + ".desktop"
    safe_rm(path, dry_run)

def rpg_menu_entry(destdir, package_name, title, rpg_file, prefix = "/usr"):
    command = "%s/games/ohrrpgce-game %s/share/games/%s/%s" % (prefix, prefix, package_name, rpg_file)
    menu_entry(destdir, package_name, title, command, prefix = prefix)

def get_srcdir():
    if os.path.isfile('ohrrpgce-game'):
        return ""
    elif os.path.isfile('../ohrrpgce-game'):
        return "../"
    else:
        raise Exception("Can't find ohrrpgce-game and other files")

package_name = "ohrrpgce"

def install(destdir = '', prefix = '/usr', dry_run = False):
    """Installs the OHRRPGCE on the local machine (not including Vikings of Midgard).
    Pass destdir to install into a staging area instead of writing to /
    (dry_run is not implemented)."""
    files = engine_files('linux', 'minimal', get_srcdir())
    build_tree(destdir, package_name, files, prefix = prefix)
    menu_entry(destdir, package_name, "OHRRPGCE Game Player", prefix + "/games/ohrrpgce-game", desktop_file_suffix="-game", icon="ohrrpgce-game.png", prefix=prefix)
    menu_entry(destdir, package_name, "OHRRPGCE Custom Editor", prefix + "/games/ohrrpgce-custom", append=True, desktop_file_suffix="-custom", icon="ohrrpgce-custom.png", prefix=prefix)

def uninstall(destdir = '', prefix = '/usr', dry_run = False):
    """Uninstalls the OHRRPGCE from the local machine (not including Vikings of Midgard).
    Pass destdir to remove a staging area instead of writing to / (probably useless)"""
    if not os.path.isdir(destdir + prefix + "/share/games/" + package_name):
        # We might still remove other files, if it's partially installed for some reason
        print("uninstall: " + package_name + " doesn't seem to be installed. Did you specify the right prefix and destdir?")
    rm_tree(destdir, package_name, files, prefix = prefix, dry_run = dry_run)
    remove_menu_entry(destdir, package_name, desktop_file_suffix="-game", prefix=prefix, dry_run=dry_run)
    remove_menu_entry(destdir, package_name, desktop_file_suffix="-custom", prefix=prefix, dry_run=dry_run)


############################################################################
## Debian (.deb) packaging

def calculate_size(files):
    size = 0
    for path in files.datafiles + files.executables + files.icons:
        size += os.stat(files.abspath(path)).st_size
    return size / 1000

def read_version():
    codename, branch_name, branch_rev = ohrbuild.read_codename_and_branch(rootdir)
    rev, date = ohrbuild.query_svn_rev_and_date(rootdir)
    year = date[0:4]
    month = date[4:6]
    day = date[6:8]
    return "%s.%s.%s.%s-%s" % (year, month, day, branch_name, rev)

def write_control_file(filename, template, values):
    "Write /DEBIAN/control"
    f = open(filename, 'wb')
    # The file must be UTF8-encoded
    f.write((template % values).encode('utf8'))
    f.close()

def run_dpkg(outdir, package, ver):
    temp = "fakeroot dpkg -b %s %s%s_%s_amd64.deb" % (package, outdir, package, ver)
    os.system(temp.encode(sys.getfilesystemencoding()))

def create_dpkg(outdir):
    "Creates ohrrpgce_${version}_amd64.deb in the current directory"

    files = engine_files('linux', 'minimal', get_srcdir())

    maintainer = '"OHRRPGCE Development Team" <ohrrpgce@lists.motherhamster.org>'
    # For gfx_sdl builds:  libsdl-mixer1.2 (>= 1.2), libsdl1.2debian (>> 1.2)
    depends = "libc6 (>= 2.14), libncurses5 (>= 5.4), libsdl2-mixer-2.0-0 (>= 2.0.1), libsdl2-2.0-0 (>= 2.0.5), libx11-6, libxext6, libxpm4, libxrandr2, libxrender1"
    recommends = "madplay, vorbis-tools"
    version = read_version()

    safe_rmtree(package_name)
    quiet_mkdir(package_name + '/DEBIAN')
    write_control_file(package_name + '/DEBIAN/control',
    """Package: %s
Priority: optional
Section: games
Installed-Size: %d
Maintainer: %s
Architecture: amd64
Version: %s
Depends: %s
Recommends: %s
Description: Official Hamster Republic Role Playing Game Construction Engine
 The O.H.R.RPG.C.E, which stands for Official Hamster Republic Role
 Playing Game Construction Engine. The OHRRPGCE is a free utility that
 you can use to create your own RPG game in a style similar to the
 classic Final Fantasy games on the NES and SNES which have so shaped the
 RPG genre. To learn how to create your own game, or to download other
 people's games, visit http://HamsterRepublic.com/ohrrpgce/
"""
    , (package_name, calculate_size(files), maintainer, version, depends, recommends))
    install(package_name)
    run_dpkg(outdir, package_name, version)
    safe_rmtree(package_name)

############################################################################

if __name__ == '__main__':
    # Create ohrrpgce_..._amd64.deb
    # One optional arg: directory in which to place the .deb
    outdir = ''
    if len(sys.argv) >= 2:
        outdir = sys.argv[1] + '/'
    create_dpkg(outdir)
