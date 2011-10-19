#!/usr/bin/env python
import os
import sys
import time
import cPickle as pickle
import numpy as np
from nohrio.ohrrpgce import *
from nohrio.dtypes import dt
from nohrio.wrappers import OhrData
from nohrio.scripts import *
from rpgbatch import RPGIterator, RPGInfo

if len(sys.argv) < 2:
    sys.exit("Specify .rpg files, .rpgdir directories, .zip files, or directories containing any of these as arguments.")
things = sys.argv[1:]

rpgidx = np.ndarray(shape = 0, dtype = RPGInfo)
cmdcounts = np.ndarray(shape = (0, 1000), dtype = np.int32)

def iter_script_tree(root):
    yield root
    for arg in root.args():
        # Wow! Passing each item back up the call chain has got to be inefficient!
        for ret in iter_script_tree(arg):
            yield ret

commandnames = []

rpgs = RPGIterator(things)
for rpg, gameinfo, zipinfo in rpgs:
    gameinfo.has_source = False
    gameinfo.scripts_backup = False
    if zipinfo:
        gameinfo.has_source = (len(zipinfo.scripts) > 0)
    gameinfo.loadname(rpg)
    rpgidx = np.append(rpgidx, gameinfo)

    cmdusage = np.ndarray((1, 1000), np.int32)
    cmdusage.fill(0)

    hspfile = rpg.lump_path('.hsp')
    gameinfo.has_scripts = os.path.isfile(hspfile)
    if gameinfo.has_scripts:
        scripts = HSScripts(hspfile)
        gameinfo.scripts_backup = scripts.source

        # Use whichever commands.bin lump has the most in it
        if len(scripts.commands_info) > len(commandnames):
            commandnames = scripts.commands_info

        for id in scripts.scriptnames.iterkeys():
            for node in iter_script_tree(scripts.script(id)):
                if node.kind == kCmd:
                    cmdusage[0][node.id] += 1
                    if node.id == 240:  # stringfromtable
                        print "Found in", scripts.scriptnames[id], "in", gameinfo.name, ":"
                        print node
        del node
        del scripts

    cmdcounts = np.append(cmdcounts, cmdusage, axis = 0)

rpgs.print_summary()
del rpgs

rpgidx = rpgidx.view(OhrData)

with open('scriptdata.bin', 'wb') as f:
    pickle.dump({'rpgidx':rpgidx, 'cmdcounts':cmdcounts}, f)

def commandname(id):
    if id in commandnames:
        return commandnames[id]['name']
    return 'cmd%d' % id

print

tally = np.ndarray((len(rpgidx), 2, 2, 3), dtype = np.int)
tally.fill(0)
for i, r in enumerate(rpgidx):
    backup_type = 0
    if r.scripts_backup == 'source.txt':
        backup_type = 1
    elif r.scripts_backup == 'source.lumped':
        backup_type = 2
    tally[i, r.has_scripts, r.has_source, backup_type] = 1

tally = tally.sum(axis=0)

print "%d/%d games had imported scripts:" % (tally[1].sum(), len(rpgidx))
tally = tally[1]
print "                no backup   |   source.txt   |   source.lumped"
print "no orig. src       %3d             %3d                %3d" % tuple(tally[0])
print "with orig. src     %3d             %3d                %3d" % tuple(tally[1])

total = tally.sum()
print
print "%d/%d games had script source available" % (total - tally[0][0].sum(), total)

print 

cmdsums = cmdcounts.sum(axis=0)
cmdgamecounts = (cmdcounts > 0).sum(axis=0)
for i, count in enumerate(cmdsums):
    if count:
        print "%-3d: %-29s used %5d times in %3d games" % (i, commandname(i), count, cmdgamecounts[i])
