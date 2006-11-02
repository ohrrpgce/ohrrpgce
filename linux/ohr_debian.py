#!/usr/bin/env python

import os
import re
import shutil

############################################################################

def calculate_size(files, executables):
  size = 0
  for file in files:
    size += os.stat('%s' % file)[6]
  for file in executables:
    size += os.stat('%s' % file)[6]
  return size / 1000

def read_version():
  f = open('../gver.txt', 'r')
  line = f.read()
  f.close()
  regex = re.compile('.* (?P<code>[a-z]+) (?P<year>\d{4})(?P<month>\d{2})(?P<day>\d{2})"$', re.I)
  match = regex.match(line)
  year  = match.group('year')
  month = match.group('month')
  day   = match.group('day')
  code  = match.group('code')
  return "%s.%s.%s.%s-1" % (year, month, day, code)

def write_control_file(filename, template, values):
  f = open(filename, 'w')
  f.write(template % values)
  f.close()

def build_tree(dir, files, executables):
  if len(executables):
    dest = dir + "/usr/games/"
    quiet_mkdir(dest)
    for exe in executables:
      shutil.copy(exe, dest + os.path.basename(exe))
  dest = dir + "/usr/share/games/" + dir +"/"
  quiet_mkdir(dest)
  for file in files:
    shutil.copy(file, dest + os.path.basename(file))

def quiet_mkdir(dir):
  try:
    os.makedirs(dir)
  except OSError:
    pass # ignore dir-already-exists

def run_dpkg(package, ver):
  os.system("dpkg -b %s %s_%s_i386.deb" % (package, package, ver))

def menu_entry(package_name, title, command, append=False, desktop_file_suffix=""):
  mode = "w"
  if append: mode = "a"
  quiet_mkdir(package_name + "/usr/share/menu/")
  f = open(package_name + "/usr/share/menu/" + package_name, mode)
  s = '?package(%s): needs="X11" title="%s" command="%s" section="Games/RolePlaying"\n' % (package_name, title, command)
  f.write(s)
  f.close()
  quiet_mkdir(package_name + "/usr/share/applications/")
  f = open(package_name + "/usr/share/applications/" + package_name + desktop_file_suffix + ".desktop", "w")
  s = "[Desktop Entry]\nName=%s\nExec=%s\nTerminal=false\nType=Application\nCategories=Application;Game;\n" \
       % (title, command)
  f.write(s)
  f.close()

def rpg_menu_entry(package_name, title, rpg_file):
  command = "/usr/games/ohrrpgce-game /usr/share/games/%s/%s" % (package_name, rpg_file)
  menu_entry(package_name, title, command)

############################################################################
