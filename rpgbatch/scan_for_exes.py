#!/usr/bin/env python
#
# Find .exe files in zips
import sys
from rpgbatch import RPGIterator

numexes = 0
num_custom_exe = 0

if len(sys.argv) < 2:
    sys.exit("Specify .rpg files, .rpgdir directories, .zip files, or directories containing any of these as arguments.")
things = sys.argv[1:]

rpgs = RPGIterator(things)
for rpg, gameinfo, zipinfo in rpgs:
    print "Processing RPG ", gameinfo.id, "from", gameinfo.src
    print " > ", gameinfo.longname, " --- ", gameinfo.aboutline

    print zipinfo.exes
    numexes += len(zipinfo.exes)
    for exe in zipinfo.exes:
        if 'custom' in exe.lower():
            num_custom_exe += 1
            print "################################################################Includes", exe
rpgs.print_summary()

print numexes, "zips had .exes", num_custom_exe, "zips had custom.exe"
