#!/usr/bin/env python

from ohr_debian import *

############################################################################

package_name = "wandering-hamster"
maintainer = '"James Paige" <ohrrpgce@lists.motherhamster.org>'
depends = "ohrrpgce (>=2013.04)"

prefix = "../../games/wander/"
relump(prefix + "wander.rpgdir", "wander.rpg")
files = [
  "wander.rpg",
  prefix + "wander.hss",
  prefix + "README.txt"]

executables = []

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
Description: A retro (SNES style) 2D Role Playing Game.
 Lord Hasim has been overthrown, plips run rampant, and a menacing evil
 lurks in the shadows... or is it a cactus? What hamster is spiffy enough
 to save the world? Bob!  
"""
, (package_name, calculate_size(files, executables), maintainer, version, depends))
build_tree(package_name, package_name, files, executables)
rpg_menu_entry(package_name, package_name, "Wandering Hamster", "wander.rpg")
run_dpkg(package_name, version)
os.remove("wander.rpg")
