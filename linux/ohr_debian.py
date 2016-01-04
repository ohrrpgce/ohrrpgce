#!/usr/bin/env python

"""
Routines both for installing packages on a Unix, and for creating .deb packages
"""

import os
import re
import shutil
from datetime import date

############################################################################

def calculate_size(files, executables):
  size = 0
  for file in files:
    size += os.stat('%s' % file)[6]
  for file in executables:
    size += os.stat('%s' % file)[6]
  return size / 1000

# Note: this is reimplemented in ohrbuild.py
def read_version():
  year = date.today().year
  month = date.today().month
  day = date.today().day
  rev = 0
  code = read_codename()

  date_regex = re.compile('^Last Changed Date: (?P<year>\d{4})-(?P<month>\d{2})-(?P<day>\d{2})', re.I)
  rev_regex  = re.compile('^Revision: (?P<rev>\d+)', re.I)
  got_date = False
  got_rev = False
  f = os.popen('svn info ..', 'r')
  for line in f:
    match = date_regex.match(line)
    if match != None:
      year  = match.group('year')
      month = match.group('month')
      day   = match.group('day')
      got_date = True
    match = rev_regex.match(line)
    if match != None:
      rev = match.group('rev')
      got_rev = True
  f.close()
  if not got_rev: print "Failed to get subversion revision number, using 0"
  if not got_date: print "Failed to get subversion last-modified date, using today's date "
  return "%s.%s.%s.%s-%s" % (year, month, day, code, rev)

def read_codename():
  f = open('../codename.txt', 'r')
  lines = []
  for line in f:
    if not line.startswith('#'):
      lines.append(line.rstrip())
  f.close()
  if len(lines) != 2:
    exit('Expected two noncommented lines in codename.txt')
  return lines[0]

def write_control_file(filename, template, values):
  f = open(filename, 'w')
  f.write(template % values)
  f.close()

def build_tree(destdir, package_name, files, executables, prefix = "/usr"):
  if len(executables):
    dest = destdir + prefix + "/games/"
    quiet_mkdir(dest)
    for exe in executables:
      shutil.copy(exe, dest + os.path.basename(exe))
  dest = destdir + prefix + "/share/games/" + package_name +"/"
  quiet_mkdir(dest)
  for file in files:
    if os.path.isdir(file):
      # copy a folder
      shutil.copytree(file, dest + os.path.basename(file), ignore=shutil.ignore_patterns(".svn"))
    else:
      # copy a file
      shutil.copy(file, dest + os.path.basename(file))

def quiet_mkdir(dir):
  try:
    os.makedirs(dir)
  except OSError:
    pass # ignore dir-already-exists

def run_dpkg(package, ver):
  os.system("fakeroot dpkg -b %s %s_%s_i386.deb" % (package, package, ver))

def menu_entry(destdir, package_name, title, command, append=False, desktop_file_suffix="", prefix = "/usr"):
  mode = "w"
  if append: mode = "a"
  quiet_mkdir(destdir + prefix + "/share/menu/")
  f = open(destdir + prefix + "/share/menu/" + package_name, mode)
  s = '?package(%s): needs="X11" title="%s" command="%s" section="Games/RolePlaying"\n' % (package_name, title, command)
  f.write(s)
  f.close()
  quiet_mkdir(destdir + prefix + "/share/applications/")
  f = open(destdir + prefix + "/share/applications/" + package_name + desktop_file_suffix + ".desktop", "w")
  s = "[Desktop Entry]\nName=%s\nExec=%s\nTerminal=false\nType=Application\nCategories=Application;Game;\n" \
       % (title, command)
  f.write(s)
  f.close()

def rpg_menu_entry(destdir, package_name, title, rpg_file, prefix = "/usr"):
  command = "%s/games/ohrrpgce-game %s/share/games/%s/%s" % (prefix, prefix, package_name, rpg_file)
  menu_entry(destdir, package_name, title, command, prefix = prefix)

def relump(lumpdir, rpgfile):
  try:
    os.remove(rpgfile)
  except(OSError):
    # don't care if the file does not already exist
    pass
  os.system('../relump "' + lumpdir + '" "' + rpgfile + '"')

############################################################################
