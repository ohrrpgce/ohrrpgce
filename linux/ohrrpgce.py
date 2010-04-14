#!/usr/bin/env python

from ohr_debian import *

############################################################################

package_name = "ohrrpgce"
maintainer = '"OHRRPGCE Development Team" <ohrrpgce@lists.motherhamster.org>'

prefix = "../"
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
               prefix + "ohrrpgce-custom"]

depends = "libc6 (>= 2.3), libncurses5 (>= 5.4), libsdl-mixer1.2 (>= 1.2), libsdl1.2debian (>> 1.2), libx11-6, libxext6, libxpm4, libxrandr2, libxrender1"
recommends = "madplay, vorbis-tools"

############################################################################

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
build_tree(package_name, files, executables)
menu_entry(package_name, "OHRRPGCE Game Player", "/usr/games/ohrrpgce-game", desktop_file_suffix="-game")
menu_entry(package_name, "OHRRPGCE Custom Editor", "/usr/games/ohrrpgce-custom", append=True, desktop_file_suffix="-custom")
run_dpkg(package_name, version)
