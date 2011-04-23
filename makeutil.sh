#!/bin/sh
fbc -lang deprecated -v unlump.bas util.bas lumpfile.bas os_unix.bas common_base.bas vector.bas array.o blit.o || exit 1
fbc -lang deprecated -v relump.bas util.bas lumpfile.bas os_unix.bas common_base.bas vector.bas array.o blit.o || exit 1
euc hspeak.exw || exit 1
