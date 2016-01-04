#!/usr/bin/env python

from ohr_debian import *

############################################################################

package_name = "vikings-of-midgard"
maintainer = '"Fenrir Lunaris & OHR Developers" <ohrrpgce@lists.motherhamster.org>'
depends = "ohrrpgce (>=2013.04)"

prefix = "../vikings/"
relump(prefix + "vikings.rpgdir", "vikings.rpg")
files = [
  "vikings.rpg",
  prefix + "README-vikings.txt",
  prefix + "Vikings script files/viking.txt",
  prefix + "Vikings script files/viking.hsi",
  prefix + "Vikings script files/utility.lib"]

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
 The viking Gods have been too lax in their rulership of the
 world, and a shadowy menace prepares to usurp power from them.
 It will take all your strength, courage, and wits to stop this
 dastardly plan!  
 Featuring glorious pixelation artwork by Fenrir-Lunaris.
"""
, (package_name, calculate_size(files, executables), maintainer, version, depends))
build_tree(package_name, package_name, files, executables)
rpg_menu_entry(package_name, package_name, "Vikings of Midgard", "vikings.rpg")
run_dpkg(package_name, version)
os.remove("vikings.rpg")
