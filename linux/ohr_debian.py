"""
Routines both for installing packages on a Unix, and for creating .deb packages
"""

from __future__ import print_function
import sys
import os
import shutil

sys.path.append('..')
import ohrbuild

############################################################################

def calculate_size(files, executables):
  size = 0
  for file in files:
    size += os.stat('%s' % file)[6]
  for file in executables:
    size += os.stat('%s' % file)[6]
  return size / 1000

def read_version():
  codename, branch_rev = ohrbuild.read_codename_and_branchrev('..')
  rev, date = ohrbuild.query_svn_rev_and_date('..')
  year = date[0:4]
  month = date[4:6]
  day = date[6:8]
  return "%s.%s.%s.%s-%s" % (year, month, day, codename, rev)

def write_control_file(filename, template, values):
  f = open(filename, 'w')
  f.write(template % values)
  f.close()

def copy_file_or_dir(src, dest):
  """Copy src to dest. If src is a directory acts recursively,
  while preserving any existing files in dest.
  Because shutil.copytree fails if a directory already exists.
  """
  if os.path.isfile(src):
    shutil.copy2(src, dest)
  else:
    quiet_mkdir(dest)
    for filename in os.listdir(src):
      if filename == ".svn":
        continue
      srcpath = os.path.join(src, filename)
      destpath = os.path.join(dest, filename)
      copy_file_or_dir(srcpath, destpath)

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

def safe_rm(path, dry_run = False):
  assert len(path) > 5
  if os.path.isfile(path):
    print("rm", path)
    if not dry_run:
      os.remove(path)

def safe_rmtree(path, dry_run = False):
  assert len(path) > 5
  if os.path.isdir(path):
    print("rmtree", path)
    if not dry_run:
      shutil.rmtree(path)

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

def quiet_mkdir(dir):
  try:
    os.makedirs(dir)
  except OSError:
    pass # ignore dir-already-exists

def run_dpkg(package, ver):
  os.system("fakeroot dpkg -b %s %s_%s_amd64.deb" % (package, package, ver))

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

def relump(lumpdir, rpgfile):
  try:
    os.remove(rpgfile)
  except(OSError):
    # don't care if the file does not already exist
    pass
  os.system('../relump "' + lumpdir + '" "' + rpgfile + '"')

############################################################################
