#!/usr/bin/env python

url = "http://www.slimesalad.com/forum/gamedump.php"
mirrordir = "./slimesalad/"

#----------------------------------------------------------------------

import os
import urllib2
from datetime import datetime
import time
import re

#######################################################################

class GameMirrorer(object):

    def __init__(self, game, local):
        local.mirror_game(game)

#######################################################################

class GameInfo(object):

    def __init__(self, chunk):
        self.name = chunk[0]
        self.author = chunk[1]
        self.url = chunk[2]
        self.files = []
        self.pics = []
        chunk = chunk[3:]
        while len(chunk) > 0:
            gf = GameFile(self, chunk)
            chunk = chunk[3:]
            if gf.is_pic():
                self.pics.append(gf)
            else:
                self.files.append(gf)

    def serialize(self):
        s = "%s\n%s\n%s\n" % (self.name, self.author, self.url)
        for gf in self.pics + self.files:
            s += gf.serialize()
        return s

    def file_by_name(self, name):
        for gf in self.files + self.pics:
            if gf.name == name:
                return gf
        return None

    def file_by_basename(self, name):
        for gf in self.files + self.pics:
            if os.path.basename(gf.url) == name:
                return gf
        return None

#######################################################################

class GameFile(object):

    def __init__(self, game, chunk):
        self.game = game
        self.name = chunk[0]
        self.url = chunk[1]
        timestamp = float(chunk[2])
        self.date = datetime.fromtimestamp(timestamp)

    def is_pic(self):
        name = self.name.lower()
        for ext in ["png", "jpg", "jpeg", "gif", "bmp"]:
            if name.endswith("." + ext):
                return True
        return False

    def serialize(self):
        s = "%s\n%s\n%d\n" % (self.name, self.url, time.mktime(self.date.timetuple()))
        return s

#######################################################################

class GameDumpReader(object):

    def __init__(self, url, local):
        #print "Fetching ", url
        f = urllib2.urlopen(url)
        cr = ChunkReader(f)
        for chunk in cr.each():
           game = GameInfo(chunk)
           GameMirrorer(game, local)

#######################################################################

class ChunkReader(object):

    def __init__(self, f):
        self.f = f

    def get_next_chunk(self):
        lines = []
        while True:
            line = self.f.readline()
            if line:
                line = line.strip()
            if not line:
                break
            lines.append(line)
        if len(lines) == 0:
            return None
        return lines

    def each(self):
        chunk = self.get_next_chunk()
        while chunk:
            yield chunk
            chunk = self.get_next_chunk()

#######################################################################

class LocalGameCache(object):

    def __init__(self, mirrordir):
        self.dir = os.path.expanduser(mirrordir)

    def dest(self, gf):
        filename = os.path.basename(gf.url)
        return os.path.join(self.dir, filename)

    def mirror_game(self, game):
        self.download_game_files(game)
        self.save_metadata(game)

    def download_game_files(self, game):
        for gf in game.files:
            self.download_game_file(gf)

    def download_game_file(self, gf):
        if self.needs_download(gf):
            dest = self.dest(gf)
            download(gf.url, dest)

    def needs_download(self, gf):
        game = gf.game
        meta = self.load_metadata(game)
        if not meta:
            return True
        mf = meta.file_by_basename(os.path.basename(gf.url))
        if not mf:
            return True
        return gf.date > mf.date

    def metadata_file(self, game):
        metadir = os.path.join(self.dir, "metadata")
        if not os.path.isdir(metadir):
            os.mkdir(metadir)
        filename = safe_filename(game.name) + ".txt"
        return os.path.join(metadir, filename)

    def save_metadata(self, game):
        metafile = self.metadata_file(game)
        f = open(metafile, "w")
        f.write(game.serialize())
        f.close()

    def load_metadata(self, game):
        metafile = self.metadata_file(game)
        if not os.path.isfile(metafile):
            return None
        f = open(metafile, "r")
        chunk = f.readlines()
        chunk = [line.strip() for line in chunk]
        f.close()
        return GameInfo(chunk)
        

#######################################################################

def download(url, local_file):
    print "downloading %s" % (os.path.basename(local_file))
    input = urllib2.urlopen(url)
    output = open(local_file, "wb")
    output.write(input.read())
    output.close()
    input.close()

#----------------------------------------------------------------------

__safe_filename_char = re.compile(r"^[A-Za-z0-9._-]$")
def safe_filename(filename):
    result = ""
    for ch in filename:
        if __safe_filename_char.match(ch):
            result += ch
        else:
            result += "_"
    return result

#######################################################################

local = LocalGameCache(mirrordir)
dump = GameDumpReader(url, local)
