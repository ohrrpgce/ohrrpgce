#!/usr/bin/env python
import os
import sys
import time
import numpy as np
from nohrio.ohrrpgce import *
from rpgbatch import RPGIterator

if len(sys.argv) < 2:
    sys.exit("Specify .rpg files, .rpgdir directories, .zip files, or directories containing any of these as arguments.")
things = sys.argv[1:]
#things = ['../../../reverseeng/']

data = []
withscripts = 0

rpgs = RPGIterator(things)
for rpg, gameinfo, zipinfo in rpgs:
    # This is just a dumb example
    print "Processing RPG ", gameinfo.id
    print "mod time =", time.ctime(gameinfo.mtime)
    print "maps =", int(rpg.general.maxmap) + 1
    print "format ver =", int(rpg.general.version)
    print "say size =", int(rpg.binsize.say)
    data.append((gameinfo.mtime, int(rpg.general.version), gameinfo.id))
rpgs.print_summary()
del rpgs

print withscripts, "games had script source"
print

data.sort()
for t, v, gameid in data:
    print time.ctime(t), " Format", v, gameid
