#!/usr/bin/env python
#
# Script to scan menus and menu items for some data

import os
import sys
import time
import cPickle as pickle
import numpy as np
from nohrio.ohrrpgce import *
from nohrio.dtypes import dt
from nohrio.wrappers import OhrData
from rpgbatch.rpgbatch import RPGIterator, RPGInfo, readbit
from collections import defaultdict

if len(sys.argv) < 2:
    sys.exit("Specify .rpg files, .rpgdir directories, .zip files, or directories containing any of these as arguments.")
rpgsources = sys.argv[1:]

totalmenus = 0
totalmenuitems = 0
missingmenus = 0
badmenus = 0
badmenuitems = 0
counts = defaultdict(int)
suspending = 0
allowing = 0
maxrows = defaultdict(int)
using_toggletag = {}

rpgs = RPGIterator(rpgsources)
for rpg, gameinfo, zipinfo in rpgs:
    gameinfo.archinym = rpg.archinym.prefix
    gameinfo.arch_version = rpg.archinym.version

    print "Processing RPG ", gameinfo.id, "from", gameinfo.src
    print " > ", gameinfo.longname, " --- ", gameinfo.aboutline

    try:
        if rpg.lump_size('menus.bin') == 0:  #Otherwise get a ValueError
            raise IOError
        menus = rpg.flexidata('menus.bin')
    except (ValueError, AssertionError) as e:
        print e
        badmenus += 1
        continue
    except IOError as e:
        print e
        missingmenus += 1
        continue

    try:
        menuitems = rpg.flexidata('menuitem.bin')
    except (ValueError, AssertionError,IOError) as e:
        print e
        badmenuitems += 1
        continue

    thisgame = set()

    for menu in menus:
        totalmenus += 1
        maxrows[menu['maxrows']] += 1
        if readbit(menu['bitsets'], 2):  #allow gameplay & scripts
            allowing += 1
            if readbit(menu['bitsets'], 3):  #suspend player even if gameplay allowed
                suspending += 1

    for miid, menuitem in enumerate(menuitems):
        totalmenuitems += 1
        for field in 'tagcond1 tagcond2 settag toggletag'.split():
            if menuitem[field]:
                counts[field] += 1
        if menuitem['toggletag']:
            thisgame.add(menuitem['membership'] - 1)  # The menu ID

    if thisgame:
        using_toggletag[gameinfo.id] = list(thisgame)

rpgs.print_summary()
del rpgs

print
print len(using_toggletag), "games using toggletag:"
for gameinfo, menus in using_toggletag.items():
    print gameinfo, menus

print
print "COUNTS:"
print "num menus:", totalmenus, "menu items:", totalmenuitems, "missing menus.bin", missingmenus, "bad menus.bin:", badmenus, "bad menuitem.bin:", badmenuitems
print "menus: allowing gameplay:", allowing, "and also suspending player:", suspending
for field,count in counts.items():
    print field, count
print "Menu maxrows:"
for rows,count in sorted(maxrows.items()):
    print "maxrows=%d: %d" % (rows, count)

