#!/usr/bin/env python3

"""
Check whether a game has any maps with tile/wall/foe lumps that are the wrong size
(file size, BSAVE header, and map size don't match), which afflicted a number
of very early OHRRPGCE games.

 ./checkmaps.py [game.rpgdir]
Process an .rpgdir; or else all .rpgdirs in the current directory.
"""


import sys
import os

def i16(dat, off):
    return (dat[off + 1] << 8) | dat[off]

def proc_lump(path, expect_size = None):
    with open(path, 'rb') as fh:
        dat = fh.read()
        file_len = len(dat)
        is_bsave = dat[0] == 253
        if not is_bsave:
            print(path, "Not a BSAVE file")
        else:
            pass #print(path, "BSAVE")
        bsave_len = i16(dat, 5)
        wide = i16(dat, 7)
        high = i16(dat, 9)
        tile_len = wide * high
        if len(set([file_len, bsave_len + 7, tile_len + 11])) > 1:
            if is_bsave:
                extra = ""
                if file_len != bsave_len + 7:
                    extra = "non-bsave garbage: %d" % (file_len - (bsave_len + 7))
                print(path, "%d*%d tiles, bsave %d long by %d" % (wide, high, bsave_len, bsave_len - (4 + wide * high)), extra)
                nonzero = sum(x != 0 for x in dat[11 + wide * high:])
                if nonzero:
                    print("... there are %d nonzero bytes" % nonzero)
            else:
                layers = (file_len - 11) // (wide * high)
                if (file_len - 11) % (wide * high):
                    print("%d*%d tiles, %d layers, file long by %d" % (wide, high, layers, (file_len - 11 - wide * high * layers)))
        if expect_size and (wide,high) != expect_size:
            print(path, "%d*%d instead of expected %s" % (wide, high, expect_size))
        return wide, high

def proc_game(path):
    files = os.listdir(path)
    for f in files:
        if f.endswith('.gen'):
            archinym = f[:-4]
    for mapnum in range(99):
        def lumpname(code):
            return path + '/' + archinym + '.%s%02d' % (code, mapnum)
        if not os.path.isfile(lumpname('t')):
            print(mapnum, "maps")
            return
        else:
            #print(lumpname)
            size = proc_lump(lumpname('t'))
            proc_lump(lumpname('p'), size)
            proc_lump(lumpname('e'), size)


if len(sys.argv) > 1:
    proc_game(sys.argv[1])
    sys.exit()

dirs = os.listdir('.')
for d in dirs:
    if d.endswith('.rpgdir') or d.endswith('.unlmp'):
        print(d)
        proc_game(d)
