#!/usr/bin/env python3

"""
For installing/uninstalling the OHRRPGCE locally system-wide under Linux and creating the ohrrpgce .deb package.
"""

from __future__ import print_function
import sys
import os
import shutil

# Location of ohrbuild.py, codename.txt, etc
rootdir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))

sys.path.append(rootdir)
import ohrbuild
# For utility functions and file lists
from ohrpackage import *

############################################################################
## Installing and uninstalling system-wide on Linux

def build_tree(destdir, package_name, files, executables, icons, prefix = "/usr"):
  # Executables
  if len(executables):
    dest = destdir + prefix + "/games/"
    quiet_mkdir(dest)
    for exe in executables:
      shutil.copy(exe, dest + os.path.basename(exe))
  # Icons
  if len(icons):
    # Icons should be either .png or .svg.
    # My reading of the freedesktop.org standard is that it doesn't matter where you put the icon
    # if you only have one resolution.
    dest = destdir + prefix + "/share/icons/hicolor/32x32/apps/"
    quiet_mkdir(dest)
    for icon in icons:
      shutil.copy(icon, dest + os.path.basename(icon))
  # Data files
  dest = destdir + prefix + "/share/games/" + package_name +"/"
  quiet_mkdir(dest)
  for file in files:
    copy_file_or_dir(file, dest + os.path.basename(file))

def rm_tree(destdir, package_name, files, executables, icons, prefix = "/usr", dry_run = False):
  # Executables
  if len(executables):
    dest = destdir + prefix + "/games/"
    for exe in executables:
      path = dest + os.path.basename(exe)
      safe_rm(path, dry_run)
  # Icons
  if len(icons):
    # Icons should be either .png or .svg.
    # My reading of the freedesktop.org standard is that it doesn't matter where you put the icon
    # if you only have one resolution.
    dest = destdir + prefix + "/share/icons/hicolor/32x32/apps/"
    for icon in icons:
      path = dest + os.path.basename(icon)
      safe_rm(path, dry_run)
  # Data files
  dest = destdir + prefix + "/share/games/" + package_name +"/"
  safe_rmtree(dest, dry_run)

def menu_entry(destdir, package_name, title, command, append=False, desktop_file_suffix="", prefix = "/usr", icon = None):
  mode = "w"
  if append: mode = "a"
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
    if not os.path.isdir(destdir + prefix + "/share/games/" + package_name):
        # We might still remove other files, if it's partially installed for some reason
        print("uninstall: " + package_name + " doesn't seem to be installed. Did you specify the right prefix and destdir?")
    rm_tree(destdir, package_name, files, executables, icons, prefix = prefix, dry_run=dry_run)
    remove_menu_entry(destdir, package_name, desktop_file_suffix="-game", prefix=prefix, dry_run=dry_run)
    remove_menu_entry(destdir, package_name, desktop_file_suffix="-custom", prefix=prefix, dry_run=dry_run)


############################################################################
## Debian (.deb) packaging

def calculate_size(files, executables):
  size = 0
  for file in files:
    size += os.stat('%s' % file)[6]
  for file in executables:
    size += os.stat('%s' % file)[6]
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

def run_dpkg(package, ver):
  temp = "fakeroot dpkg -b %s %s_%s_amd64.deb" % (package, package, ver)
  os.system(temp.encode(sys.getfilesystemencoding()))

package_name = "ohrrpgce"
maintainer = '"OHRRPGCE Development Team" <ohrrpgce@lists.motherhamster.org>'

# For gfx_sdl builds:  libsdl-mixer1.2 (>= 1.2), libsdl1.2debian (>> 1.2)
depends = "libc6 (>= 2.14), libncurses5 (>= 5.4), libsdl2-mixer-2.0-0 (>= 2.0.1), libsdl2-2.0-0 (>= 2.0.5), libx11-6, libxext6, libxpm4, libxrandr2, libxrender1"
recommends = "madplay, vorbis-tools"

def create_dpkg():
    "Creates ohrrpgce_${version}_amd64.deb"
    version = read_version()
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
    , (package_name, calculate_size(files, executables), maintainer, version, depends, recommends))
    install(package_name)
    run_dpkg(package_name, version)

############################################################################

if __name__ == '__main__':
    create_dpkg()
