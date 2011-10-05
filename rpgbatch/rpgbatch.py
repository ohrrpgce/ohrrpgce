import os
import sys
import numpy as np
from nohrio.ohrrpgce import *
from nohrio.rpg2 import RPG
#from nohrio.ohrstring import *
import zipfile
from tempfile import mkdtemp
from weakref import proxy
import shutil
import hashlib
import calendar
import time
import traceback
import re

path = os.path

def file_ext_in(name, *types):
    return path.splitext(name)[1].lower()[1:] in types

def is_rpg(name):
    return file_ext_in(name, 'rpg', 'rpgdir')

top_level = ["defineconstant", "definetrigger", "defineoperator", 
             "globalvariable", "definefunction", "definescript", "include"]

def is_script(f):
    "Given a (text) file try to determine whether is it contains hamsterspeak scripts"

    with filename_or_handle(f, 'r') as f:
        # Read up to the first top level token, ignoring # comments
        for line in f:
            match = re.search('[,()#]', line)
            if match:
                line = line[:match.start()]
            line = line.lower().strip().replace(' ', '')
            if line:
                return line in top_level
    return False

class RPGInfo(object):
    pass

class ArchiveInfo(object):
    pass

class RPGIterator(object):
    def __init__(self, things):
        """Pass in a list of paths: .rpg files, .rpgdir folders, .zip files containing the preceding, folders containing the preceding.

        Also, you may pass in strings prefixed with 'src:' which sets the gameinfo.src tag for the following games."""

        self.cur_src = ''
        self.zipfiles = []
        self.rpgfiles = []
        self.hashs = {}
        self.cleanup_dir = False
        self.current_rpg = None

        # Stats
        self.num_badzips = 0
        self.num_rpgs = 0
        self.num_uniquerpgs = 0
        self.bytes = 0

        for arg in things:
            if arg.startswith('src:'):
                self.cur_src = arg[4:]
            elif path.isdir(arg):
                if arg.lower().endswith(".rpgdir"):
                    self._addfile(path.abspath(arg))
                else:
                    for node in os.listdir(arg):
                        self._addfile(path.join(arg, node))
            elif path.isfile(arg):
                if not self._addfile(arg):
                    print "Unrecognised file '%s'" % arg
            else:
                print "Unrecognised argument", arg

    def _addfile(self, node):
        if node.lower().endswith(".zip"):
            self.zipfiles.append((path.abspath(node), self.cur_src))
            return True
        if is_rpg(node):
            self.rpgfiles.append((path.abspath(node), self.cur_src))
            return True

    def _nonduplicate(self, fname, gameid):
        self.num_rpgs += 1
        md5 = hashlib.md5()
        with open(fname, 'rb') as f:
            while True:
                data = f.read(8192)
                if not data:
                    break
                md5.update(data)
        digest = md5.digest()
        if digest in self.hashs:
            self.hashs[digest].append(gameid)
            return False
        else:
            self.hashs[digest] = [gameid]
            self.num_uniquerpgs += 1
            return True

    def _get_rpg(self, fname):
        rpg = RPG(fname)
        if path.isfile(fname):
            # unlump; rpg2.RPGFile isn't very useful
            fname = path.join(self.tmpdir, path.basename(fname) + "dir")
            os.mkdir(fname)
            for foo in rpg.unlump_all(fname):
                #sys.stdout.write('.')
                pass
            rpg = RPG(fname)
            self.cleanup_dir = fname
        # We return a proxy because otherwise after being yielded from __iter__ and
        # bound to a variable, it won't be unbound from that variable and deleted
        # until after the next rpg is yielded --- meaning _cleanup() won't work.
        self.current_rpg = rpg
        return proxy(rpg)

    def _cleanup(self):
        self.current_rpg = None
        if self.cleanup_dir:
            shutil.rmtree(self.cleanup_dir, ignore_errors = True)
            self.cleanup_dir = False

    def __iter__(self):
        self.timer = time.time()
        self.tmpdir = mkdtemp(prefix = "gamescanner_tmp")
        try:
            for fname, src in self.rpgfiles:
                if self._nonduplicate(fname, fname):
                    gameinfo = RPGInfo()
                    gameinfo.rpgfile = path.basename(fname)
                    gameinfo.id = fname
                    gameinfo.src = src
                    gameinfo.mtime = os.stat(fname).st_mtime
                    gameinfo.size = os.stat(fname).st_size
                    self.bytes += gameinfo.size
                    yield self._get_rpg(fname), gameinfo, None
                    self._cleanup()

            for f, src in self.zipfiles:
                # ZipFile is a context manager only in python 2.7+
                try:
                    archive = zipfile.ZipFile(f, "r")
                    #if archive.testzip():
                    #    raise zipfile.BadZipfile

                    zipinfo = ArchiveInfo()
                    zipinfo.zip = proxy(archive)
                    zipinfo.scripts = []
                    zipinfo.exes = []
                    zipinfo.rpgs = []

                    # First scan for interesting stuff
                    for name in archive.namelist():
                        if is_rpg(name):
                            zipinfo.rpgs.extend(name)
                        elif file_ext_in(name, 'hss', 'txt'):
                            source = archive.open(name)
                            if is_script(source):
                                zipinfo.scripts.extend(name)
                            source.close()
                        elif file_ext_in(name, 'exe'):
                            zipinfo.exes.extend(name)

                    for name in zipinfo.rpgs:
                        # FIXME: add rpgdir support
                        print "Found %s in %s" % (name, f)
                        fname = path.join(self.tmpdir, path.basename(name))
                        # Reimplementing ZipFile.extract, so that the target is
                        # closed immediately if interrupted by an exception
                        #archive.extract(name, self.tmpdir)
                        source = archive.open(name)
                        with open(fname, "wb") as target:
                            shutil.copyfileobj(source, target)
                        source.close()
                        gameinfo = RPGInfo()
                        gameinfo.rpgfile = path.basename(name)
                        gameinfo.id = "%s:%s" % (path.basename(f), name)
                        gameinfo.src = src
                        gameinfo.mtime = calendar.timegm(archive.getinfo(name).date_time)
                        gameinfo.size = os.stat(fname).st_size
                        if self._nonduplicate(fname, gameinfo.id):
                            self.bytes += gameinfo.size
                            yield self._get_rpg(fname), gameinfo, zipinfo
                            self._cleanup()
                        os.remove(fname)

                except zipfile.BadZipfile:
                    print f, "is corrupt, skipping"
                    self.num_badzips += 1
                finally:
                    archive.close()
                    archive = None

        finally:
            self._cleanup()
            try:
                shutil.rmtree(self.tmpdir)
            except:
                traceback.print_exc(1, sys.stderr)
                print
        self.timer = time.time() - self.timer

    def print_summary(self):
        print
        print "Scanned %d zips (%d bad)" % (len(self.zipfiles), self.num_badzips)
        print "Found %d RPGS (%d unique, totalling %.1f MB)" % (self.num_rpgs, self.num_uniquerpgs, self.bytes / 2.**20)
        print "Finished in %.2f s" % self.timer
        for gameids in self.hashs.itervalues():
            if len(gameids) > 1:
                print "The following are identical:", gameids
        print
