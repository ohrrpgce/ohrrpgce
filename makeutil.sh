#!/bin/sh
fbc -lang deprecated -v unlump.bas util.bas blit.o || exit 1
fbc -lang deprecated -v relump.bas util.bas blit.o || exit 1
ecu hspeak.exw
./emake
