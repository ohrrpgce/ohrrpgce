#!/usr/bin/env python
import sys
import time
from rpgbatch import RPGIterator

if len(sys.argv) < 2:
    sys.exit("Specify .rpg files, .rpgdir directories, .zip files, or directories containing any of these as arguments.")

data = []

rpgs = RPGIterator(sys.argv[1:])
for rpg, gameid, mtime in rpgs:
    print "Processing RPG ", gameid
    print "mod time =", time.ctime(mtime)
    print "maps =", int(rpg.general.maxmap) + 1
    print "format ver =", int(rpg.general.version)
    data.append((mtime, int(rpg.general.version), gameid))
rpgs.print_summary()
del rpgs

data.sort()
for t, v, gameid in data:
    print time.ctime(t), " Format", v, gameid
