#!/bin/sh
fbc -lang deprecated -v unlump.bas util.bas lumpfile.bas blit.o || exit 1
fbc -lang deprecated -v relump.bas util.bas lumpfile.bas blit.o || exit 1
if [ `which ecu` ] ; then
  ecu hspeak.exw
  ./emake
fi
