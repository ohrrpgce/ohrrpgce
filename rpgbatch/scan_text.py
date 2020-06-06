#!/usr/bin/env python
#
# Script to scan for strings in .rpgs. It isn't complete. Strings in the following
# are scanned:
# -textboxes
# -global text strings
# -items
# -attacks
# -scripts
# Anything else, including slice collections, map names, heroes and enemies aren't.
#
# The script checks for various sequences that look like markup, counts and prints them.

import re
import os
import sys
import time
import cPickle as pickle
import numpy as np
from nohrio.ohrrpgce import *
from nohrio.dtypes import dt
from nohrio.wrappers import OhrData
from nohrio.scripts import *
import nohrio.stt
from rpgbatch import RPGIterator, RPGInfo, readbit

if len(sys.argv) < 2:
    sys.exit("Specify .rpg files, .rpgdir directories, .zip files, or directories containing any of these as arguments.")
rpgsources = sys.argv[1:]

affectedgames = 0
hits_sq = 0
hits_ang = 0
hitslist = []

totaltextboxes = 0
badtextboxes = 0
totalattacks = 0
badattacks = 0
totalitems = 0
baditems = 0
badglobalstrings = 0
scriptnum = 0
scripts_with_strings = 0
script_strings = 0

found_escape1 = []
found_escape2 = []
found_escape3 = []
found_escape4 = []
found_escape5 = []
found_chars = []

regex = re.compile(r"\[[./]([a-zA-Z][-?a-zA-Z0-9=.,:]*)\]")
regex2 = re.compile(r"<[./]([a-zA-Z][-?a-zA-Z0-9=.,:]*)>")

#codes = ('i', 'it', 'italic', 'italics', 'b', 'bo', 'bold', 'u', 'ul', 'wave', 'rumble', 'r', 'w', 'sp', 'space', 'spc',
#         'ic', 'ico', 'icon', 'lm', 'rm', 'c', 'col', 'color', 'colour', 'f', 'font', 'bg', 'wait', 'w', 'h', 'hl')

def process_text(line, code):
    "Called for each string in the game. 'code' identifiers the type of the source."
    global hits_sq, hits_ang, found_escape1, found_escape2, found_escape3, found_escape4, found_escape5, thisgame
    for match in regex.finditer(line):
        hits_sq += 1
        thisgame[match.group(1)] = code + "  " + line
    for match in regex2.finditer(line):
        hits_ang += 1
        thisgame[match.group(1)] = code + "  " + line

    if r"[:" in line:
        found_escape1.append(gameinfo.id + " " + code + "  " + line)
    if r"[." in line:
        found_escape2.append(gameinfo.id + " " + code + "  " + line)
    if r"[!" in line:
        found_escape3.append(gameinfo.id + " " + code + "  " + line)
    if r"<." in line:
        found_escape4.append(gameinfo.id + " " + code + "  " + line)
    if r"<!" in line:
        found_escape5.append(gameinfo.id + " " + code + "  " + line)

    for ch in line:
        if 1 <= ord(ch) <= 31 and ord(ch) != 10 and ord(ch) != 13:
            found_chars.append(gameinfo.id + " " + code + "  {" + str(ord(ch)) + "} " + line)
            break

rpgs = RPGIterator(rpgsources)
for rpg, gameinfo, zipinfo in rpgs:
    gameinfo.archinym = rpg.archinym.prefix
    gameinfo.arch_version = rpg.archinym.version

    #print "Processing RPG ", gameinfo.id, "from", gameinfo.src
    #print " > ", gameinfo.longname, " --- ", gameinfo.aboutline

    thisgame = {}

    tbs = None
    try:
        tbs = rpg.flexidata('say')
    except (ValueError, AssertionError, IOError) as e:
        print "SAY:", e
        badtextboxes += 1

    if tbs is not None:
        for textid, tb in enumerate(tbs):
            totaltextboxes += 1
            for line in tb['text']:
                process_text(line, "{TB} ")
            process_text(get_str8(tb['choice1']['name']), "{CHOICE} ")
            process_text(get_str8(tb['choice2']['name']), "{CHOICE} ")

    attacks = None
    try:
        attacks = rpg.data('attack.full')
    except (ValueError, AssertionError, IOError) as e:
        print "ATTACKS:", e
        badattacks += 1

    if attacks is not None:
        for attackid, attack in enumerate(attacks):
            totalattacks += 1
            process_text(get_str16(attack['name']), "{ATKNM}")
            process_text(get_str8(attack['caption']), "{ATKCP}")
            process_text(get_str8(attack['description']), "{ATKDS}")


    items = None
    try:
        items = rpg.flexidata('itm')
    except (ValueError, AssertionError, IOError) as e:
        print "ITM;", e
        baditems += 1

    if items is not None:
        for item in items:
            totalitems += 1
            process_text(get_str16(item['name']), "{ITMNAME}")
            process_text(get_str16(item['info']), "{ITMINFO}")

    try:
        names, invalid = nohrio.stt.stt_strings(rpg)
        if invalid:
            print "INVALID:", invalid
        for strid, text in names.iteritems():
            process_text(text, "{STT%d}" % strid)
    except (ValueError, AssertionError, IOError) as e:
        print "STT:", e
        badglobalstrings += 1

    hspfile = rpg.lump_path('.hsp')
    gameinfo.has_scripts = os.path.isfile(hspfile)
    if gameinfo.has_scripts:
        scriptset = HSScripts(hspfile)
        for script_id in scriptset.scriptnames.iterkeys():
            script = scriptset.script(script_id)
            if not script:
                continue
            scriptnum += 1
            if script.strtable is not None:
                scripts_with_strings += 1
                lengths = script.strtable.view(np.uint32)
                strings = script.strtable.view(np.dtype('S%d' % len(script.strtable)))[0]
                offset = 0
                while offset < len(script.strtable):
                    length = lengths[offset // 4]
                    s = strings[offset+4 : offset+4+length]
                    #print "str", offset, length, s
                    offset += 4 + ((length + 3) // 4) * 4
                    script_strings += 1
                    process_text(s, "{SCRPT}")


    if thisgame:
        affectedgames += 1
        hitslist.append((gameinfo.id, thisgame.values()))

rpgs.print_summary()
del rpgs

print "MARKUP OCCURRENCES:"
print
for game in hitslist:
    print game[0]
    for line in game[1]:
        print line

print "SUBSEQUENCE OCCURRENCES:"
for (name, lst) in zip((r'[:', r'[.', r'[!', '<.', '<!'), (found_escape1, found_escape2, found_escape3, found_escape4, found_escape5)):
    print
    print name
    print
    for line in lst:
        print line


print "COUNTS:"
print "num textboxes:", totaltextboxes, "attacks:", totalattacks, "items:", totalitems
print "bad textboxes:", badtextboxes, "attacks:", badattacks, "items:", baditems
print "num scripts: ", scriptnum, "with strings:", scripts_with_strings, "strings:", script_strings
print "[[./]foo] hits:", hits_sq, " <[./]foo> hits:", hits_ang
print "games with [[./]foo] or <[./]foo>:", affectedgames
