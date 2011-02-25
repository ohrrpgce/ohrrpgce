#!/bin/sh
fbc -lang deprecated -v unlump.bas util.bas lumpfile.bas os_unix.bas blit.o || exit 1
fbc -lang deprecated -v relump.bas util.bas lumpfile.bas os_unix.bas blit.o || exit 1
euc hspeak.exw || exit 1
