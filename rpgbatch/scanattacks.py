#!/usr/bin/env python
#
# Script to scan attacks

import os
import sys
import time
import cPickle as pickle
import numpy as np
from nohrio.ohrrpgce import *
from nohrio.dtypes import dt
from nohrio.wrappers import OhrData
from rpgbatch import RPGIterator, RPGInfo, readbit

if len(sys.argv) < 2:
    sys.exit("Specify .rpg files, .rpgdir directories, .zip files, or directories containing any of these as arguments.")
rpgsources = sys.argv[1:]

totalattacks = 0
num_reset_attacks = 0
bitandfail = 0
reset_without_inflict = 0
affectedgames = 0
badattacks = 0

rpgs = RPGIterator(rpgsources)
for rpg, gameinfo, zipinfo in rpgs:
    gameinfo.archinym = rpg.archinym.prefix
    gameinfo.arch_version = rpg.archinym.version

    print "Processing RPG ", gameinfo.id, "from", gameinfo.src
    print " > ", gameinfo.longname, " --- ", gameinfo.aboutline

    try:
        attacks = rpg.data('attack.full')
    except (ValueError, AssertionError) as e:
        print e
        badattacks += 1
        continue

    thisgame = False

    for attackid, attack in enumerate(attacks):
        totalattacks += 1
        if readbit(attack['bitsets1'], 57):  # "reset target stat to max before hit"
            #print attack['bitsets1']
            num_reset_attacks += 1

            if readbit(attack['bitsets1'], 51):  # "show damage without inflicting"
                print "### Found attack", attackid, get_str16(attack['name']), "in", gameinfo.id, time.ctime(gameinfo.mtime)
                print "### desciption: ", get_str8(attack['description'])
                reset_without_inflict += 1
                thisgame = True
               
            if False:
                if attack['target_stat'] <= 1:  #targetting HP or MP
                    for i in range(21, 21 + 16):
                        # Check whether any of the old 16 elemental + enemy killer bits are set
                        if readbit(attack['bitsets1'], i):
                            bitandfail += 1
                            print "### Found attack", attackid, get_str16(attack['name']), "in", gameinfo.id, time.ctime(gameinfo.mtime)
                            print "### desciption: ", get_str8(attack['description'])
                            thisgame = True
                            break

    if thisgame:
        affectedgames += 1

rpgs.print_summary()
del rpgs

print "COUNTS:"
print "num attacks:", totalattacks, ' "reset targ stat" attacks:', num_reset_attacks,
print  ' ...with "show without inflicting":', reset_without_inflict
#print  ' ...with elemental fail conds:', bitandfail
print "affected games:", affectedgames, " bad attack data:", badattacks
