#!/usr/bin/env python
#
# Script to scan attacks for garbage in unused fields

import os
import sys
import time
import cPickle as pickle
import numpy as np
from nohrio.ohrrpgce import *
from nohrio.dtypes import dt
from nohrio.wrappers import OhrData
from rpgbatch.rpgbatch import RPGIterator, RPGInfo, readbit

if len(sys.argv) < 2:
    sys.exit("Specify .rpg files, .rpgdir directories, .zip files, or directories containing any of these as arguments.")
rpgsources = sys.argv[1:]


class AttackGarbageTally(object): pass
games = AttackGarbageTally()
games.garbage = games.early2006_garbage = games.bitgarbage = games.newest = games.early2006 = games.older = games.tooold = games.bad = 0

rpgs = RPGIterator(rpgsources)
for rpg, gameinfo, zipinfo in rpgs:
    # rpg is always an RPGDir
    gameinfo.archinym = rpg.archinym.prefix
    gameinfo.arch_version = rpg.archinym.version

    if not rpg.has_lump('attack.bin'):
        # Before attack captions were added attack.bin didn't exist
        games.tooold += 1
    else:
        # fixBits is a bit of a mess
        if rpg.has_lump('fixbits.bin'):
            fixbits = fixBits(rpg.lump_path('fixbits.bin'))
            fixAttackItems = fixbits.attackitems
            fixNumElements = fixbits.numelements
            del fixbits
        else:
            fixAttackItems = False
            fixNumElements = False
        if fixAttackItems:
            games.newest += 1
            # Check the highest currently unused 56 or 104 bits in the second bank of attack bitsets

            try:
                attack_bin8 = rpg.data('attack.bin', dtype = (np.uint8, rpg.binsize.attack))
            except Exception as e:
                print gameinfo.id, "threw exception when memmapping attack.bin:", e
                games.bad += 1
            else:
                if fixNumElements:
                    # Last 48 bits used for extra elements
                    endbyte = 60
                else:
                    endbyte = 66
                attackgarbage = attack_bin8[:, 53:endbyte].any(axis=1)
                if attackgarbage.any():
                    games.bitgarbage += 1
                    print gameinfo.id, "contains garbage in unused bits for attacks", attackgarbage.nonzero()[0]
                    sumbits = np.zeros(endbyte - 53, dtype = np.uint8)
                    for atk in attack_bin8:
                        sumbits |= atk[53:endbyte]
                    print "with bitmask", sumbits

        elif rpg.binsize.attack != 120 or rpg.binsize.stf < 64:
            games.bad += 1
            print gameinfo.id, "had unexpected binsizes", rpg.binsize.attack, rpg.binsize.stf

        else:
            # scan attack data

            if rpg.binsize.stf == 64:
                # A game earlier than r270 (Jan 2006) which is just after tags were added in attack_bin[:,19:25]
                games.older += 1
                begin = 19
            else:
                # A game between Jan 2006 and (July 2006) when fixAttackBit was added.
                # Scan only the stuff fixAttackBit blanks out
                games.early2006 += 1
                begin = 53

            try:
                attack_bin16 = rpg.data('attack.bin', dtype = (np.uint16, rpg.binsize.attack / 2))
            except Exception as e:
                print gameinfo.id, "threw exception when memmapping attack.bin:", e
                games.bad += 1
            else:
                scanrange = attack_bin16[:, begin:60]
                attackgarbage = scanrange.any(axis=1)
                if attackgarbage.any():
                    games.garbage += 1
                    print gameinfo.id, "contains garbage in attacks", attackgarbage.nonzero()[0], "at offsets"
                    print scanrange.any(axis=0).nonzero()[0] + begin
                    #for atk in attackgarbage.nonzero()[0]:
                    #    print "  attack", atk, scanrange[atk]

print
rpgs.print_summary()
print "Of the %d newest games, %d had garbage in the unused attack bits" % (games.newest, games.bitgarbage)
print "Of %d early-2006 games, %d had attack.bin garbage" % (games.early2006, games.early2006_garbage)
print "Of %d older games with attack.bin, %d had attack.bin garbage" % (games.older, games.garbage)
print "Of the remaining, %d did not have attack.bin, %d were bad" % (games.tooold, games.bad)
