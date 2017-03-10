#!/usr/bin/env python2

import os
import sys
import datetime
import shutil
import re

nightlies = os.path.expanduser("~/HamsterRepublic.com/ohrrpgce-static/nightly/")
archivedir = os.path.expanduser("~/rpg.hamsterrepublic.com/nightly-archive/")
now_string = datetime.date.today().strftime("%Y-%m-%d")
destdir = os.path.join(archivedir, now_string)
max_archives = 10

if os.path.exists(destdir):
    if os.path.isdir(destdir):
        print "Opps! {0} directory already exists.".format(destdir)
        sys.exit(1)
    else:
        print "Opps! {0} already exists, but it ain't a directory!".format(destdir)
        sys.exit(1)

os.mkdir(destdir)

# copy over the current nightlies
print "Copying over nightly builds into {0}".format(destdir)
for filename in os.listdir(nightlies):
    if filename in [".", "..", ".htaccess", "CONFUSED?.html", "ohrrpgce-wip-default.zip"]:
        # skip stuff that we don't want to archive
        continue
    srcfile = os.path.join(nightlies, filename)
    shutil.copy2(srcfile, destdir)
htaccess = os.path.join(destdir, ".htaccess")
f = open(htaccess, "w")
f.write("IndexOptions HTMLTable IgnoreClient NameWidth=* FoldersFirst\n")
f.close()

# erase old nightlies

# First make a list of them
dir_list = []
dir_pattern = re.compile(r"^\d{4}-\d{2}-\d{2}$")
for dirname in os.listdir(archivedir):
    if dirname in [".", ".."]:
        continue
    if dir_pattern.match(dirname):
        dir_list.append(dirname)
dir_list.sort(reverse=True)

# Then delete the oldest ones
while len(dir_list) > max_archives:
    delete_me = os.path.join(archivedir, dir_list[-1])
    dir_list = dir_list[:-1]
    print "Deleting old archive {0}".format(delete_me)
    shutil.rmtree(delete_me)
