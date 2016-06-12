#!/usr/bin/env python

"""
For creating the ohrrpgce .deb package, and also installing the OHRRPGCE locally system-wide.
"""

import os
from ohr_debian import *

############################################################################

package_name = "ohrrpgce"
maintainer = '"OHRRPGCE Development Team" <ohrrpgce@lists.motherhamster.org>'

if os.path.isfile('ohrrpgce-game'):
    prefix = ""
elif os.path.isfile('../ohrrpgce-game'):
    prefix = "../"
else:
    raise Exception("Can't find ohrrpgce-game and other files")

files = [
  prefix + "ohrrpgce.new",
  prefix + "README-game.txt",
  prefix + "README-custom.txt",
  prefix + "LICENSE.txt",
  prefix + "LICENSE-binary.txt",
  prefix + "whatsnew.txt",
  prefix + "plotscr.hsd",
  prefix + "scancode.hsi",
  prefix + "unlump",
  prefix + "relump",
  prefix + "ohrhelp"]

executables = [prefix + "ohrrpgce-game",
               prefix + "ohrrpgce-custom",
               prefix + "hspeak"]

icons = [prefix + "ohrrpgce-game.png",
         prefix + "ohrrpgce-custom.png"]

depends = "libc6 (>= 2.3), libncurses5 (>= 5.4), libsdl-mixer1.2 (>= 1.2), libsdl1.2debian (>> 1.2), libx11-6, libxext6, libxpm4, libxrandr2, libxrender1"
recommends = "madplay, vorbis-tools"

############################################################################

def create_dpkg():
    "Creates ohrrpgce_${version}_i386.deb"
    version = read_version()
    quiet_mkdir(package_name + '/DEBIAN')
    write_control_file(package_name + '/DEBIAN/control',
    """Package: %s
Priority: optional
Section: games
Installed-Size: %d
Maintainer: %s
Architecture: i386
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
    , (package_name, calculate_size(files, executables), maintainer, version, depends, recommends))
    install(package_name)
    run_dpkg(package_name, version)

def install(destdir = '', prefix = '/usr', dry_run = False):
    """Installs the OHRRPGCE on the local machine (not including Vikings of Midgard).
    Pass destdir to install into a staging area instead of writing to /
    (dry_run is not implemented)."""
    build_tree(destdir, package_name, files, executables, icons, prefix = prefix)
    menu_entry(destdir, package_name, "OHRRPGCE Game Player", prefix + "/games/ohrrpgce-game", desktop_file_suffix="-game", icon="ohrrpgce-game.png", prefix=prefix)
    menu_entry(destdir, package_name, "OHRRPGCE Custom Editor", prefix + "/games/ohrrpgce-custom", append=True, desktop_file_suffix="-custom", icon="ohrrpgce-custom.png", prefix=prefix)

def uninstall(destdir = '', prefix = '/usr', dry_run = False):
    """Uninstalls the OHRRPGCE from the local machine (not including Vikings of Midgard).
    Pass destdir to remove a staging area instead of writing to / (probably useless)"""
    rm_tree(destdir, package_name, files, executables, icons, prefix = prefix, dry_run=dry_run)
    remove_menu_entry(destdir, package_name, desktop_file_suffix="-game", prefix=prefix, dry_run=dry_run)
    remove_menu_entry(destdir, package_name, desktop_file_suffix="-custom", prefix=prefix, dry_run=dry_run)

if __name__ == '__main__':
    create_dpkg()
