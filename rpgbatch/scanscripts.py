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
    print "Pass path to plotscr.hs (plotscr.hsd compiled) as first argument to distinguish the standard scripts."
    print "Specify .rpg files, .rpgdir directories, .zip files, or directories containing any of these as arguments."
    sys.exit()
things = sys.argv[1:]

commandnames = []
scripthashes = {}

standardscrs = {'names': []}
if things[0].endswith('plotscr.hs'):
    scripts = HSScripts(things[0])
    standardscrs['names'] = scripts.scriptnames.values()
    commandnames = scripts.commands_info
    del scripts
    print "Read", len(standardscrs['names']), "standard scripts from", things[0]
    things.pop(0)
    # A few special cases for scripts which were removed from plotscr.hsd
    # (all of these were in fact replaced with builtin commands)
    for n in ('setstring', 'appendstring', 'suspendmapmusic', 'resumemapmusic'):
        if n not in standardscrs['names']:
            standardscrs['names'].append(n)
standardscrs['versions'] = [[0, 0] for x in standardscrs['names']]
standardscrs['games'] = [[] for i in range(len(standardscrs['names']))]

rpgidx = np.zeros(shape = 0, dtype = RPGInfo)
cmdcounts = np.zeros(shape = (0, 2000), dtype = np.int32)
standardscrs['cmdcounts'] = np.zeros((2000), np.int32)

def iter_script_tree2(root):
    yield root
    for arg in root.args():
        # Wow! Passing each item back up the call chain has got to be inefficient!
        for ret in iter_script_tree(arg):
            yield ret

# This is about 30% faster than iter_script_tree2... disappointing
def iter_script_tree(root):
    "Iterate over the descendents of a script node"
    node = ScriptNode(root.scripts(), root.scrinfo, root.offset)
    yield node
    if root.argnum == 0:
        return
    lst = [root.offset + 3, root.argnum]
    data = root.scrdata()
    while len(lst):
        node.offset = data[lst[-2]]
        yield node
        if lst[-1] == 1:
            lst = lst[:-2]
        else:
            lst[-2] += 1
            lst[-1] -= 1
        argnum = node.argnum
        if argnum:
            lst.append(node.offset + 3)
            lst.append(argnum)

scriptbytes = 0
scriptuniquebytes = 0
scriptnum = 0
scriptuniquenum = 0

output = ''

rpgs = RPGIterator(things)
for rpg, gameinfo, zipinfo in rpgs:
    gameinfo.has_source = False
    gameinfo.scripts_backup = False
    if zipinfo:
        gameinfo.has_source = (len(zipinfo.scripts) > 0)
    gameinfo.loadname(rpg)
    rpgidx = np.append(rpgidx, gameinfo)

    cmdusage = np.zeros((1, 2000), np.int32)

    hspfile = rpg.lump_path('.hsp')
    gameinfo.has_scripts = os.path.isfile(hspfile)
    if gameinfo.has_scripts:
        scripts = HSScripts(hspfile)
        gameinfo.scripts_backup = scripts.source

        # Use whichever commands.bin lump has the most in it
        if len(scripts.commands_info) > len(commandnames):
            commandnames = scripts.commands_info

        for id in scripts.scriptnames.iterkeys():
            script = scripts.script(id)
            if not script:
                continue
            scriptnum += 1
            md5 = script.scrdata().view(OhrData).md5()
            info = dict(script.scrinfo)
            del info['data']
            info['id'] = id
            info['name'] = scripts.scriptnames[id]
            info['game'] = gameinfo.name
            if info['name'] in standardscrs['names']:
                usagevec = standardscrs['cmdcounts']
            else:
                usagevec = cmdusage[0]
            scriptbytes += script.scrdata().nbytes
            if md5 in scripthashes:
                scripthashes[md5].append(info)
            else:
                scriptuniquenum += 1
                scripthashes[md5] = [info]
                scriptuniquebytes += script.scrdata().nbytes
                for node in iter_script_tree(script):
                    if node.kind == kCmd:
                        usagevec[node.id] += 1
                        if node.id == 240:  # stringfromtable
                            output += "Found in " + scripts.scriptnames[id] + " in " + gameinfo.name + ":\n" + str(node) + '\n'
        scripts.close()
        del scripts

    cmdcounts = np.append(cmdcounts, cmdusage, axis = 0)

rpgs.print_summary()
del rpgs
print "scanned %d scripts totalling %.2f MB (%.2f MB nonunique)" % (scriptnum, scriptuniquebytes / 2.**20, scriptbytes / 2.**20)
print
for key, info in scripthashes.iteritems():
    if len(info) > 1 and info[0]['name'] not in standardscrs['names']:
        games = []
        duptext = ''
        for i in info:
            if i['game'] not in games:
                games.append(i['game'])
                duptext += "\n   " + i['name'] + " in " + i['game']
        if len(games) > 1:
            print "Script duplicated", len(games), "times:", duptext
print
for info in scripthashes.itervalues():
    try:
        idx = standardscrs['names'].index(info[0]['name'])
    except ValueError:
        pass
    else:
        if info[0]['version'] == 0:
            standardscrs['versions'][idx][0] += 1
        else:
            standardscrs['versions'][idx][1] += 1
        standardscrs['games'][idx].extend(info)
for name, versions, games in zip(standardscrs['names'], standardscrs['versions'], standardscrs['games']):
    print "Found %d versions of %-21s in %d games" % (versions[0] + versions[1], name, len(games)), "(%d 16-bit, %d 32-bit)" % tuple(versions)

print
print output

rpgidx = rpgidx.view(OhrData)

with open('scriptdata.bin', 'wb') as f:
    pickle.dump({'rpgidx':rpgidx, 'cmdcounts':cmdcounts, 'standardscrs':standardscrs}, f)

def commandname(id):
    if id in commandnames:
        return commandnames[id]['name']
    return 'cmd%d' % id

print

tally = np.zeros((len(rpgidx), 2, 2, 3), dtype = np.int)
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
print "...of which %d/%d games had script source available" % (total - tally[0][0].sum(), total)

print 

cmdsums = cmdcounts.sum(axis=0)
cmdgamecounts = (cmdcounts > 0).sum(axis=0)
for i, count in enumerate(cmdsums):
    if count or standardscrs['cmdcounts'][i]:
        print "%-3d: %-29s %5d uses in %3d games + %2d in plotscr.hsd" % (i, commandname(i), count, cmdgamecounts[i], standardscrs['cmdcounts'][i])
