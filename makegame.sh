#!/bin/sh
GFX=${1}
MUSIC=${2}
EXTRA=

if [ -z "${GFX}" ] ; then
  GFX=fb
fi

if [ -z "${MUSIC}" ] ; then
  MUSIC=sdl
fi

if [ "${MUSIC}" = "native" ] ; then
  cd audwrap
  make
  cd ..
  #EXTRA="-p audwrap -l audwrap"
fi

fbc -lang deprecated verprint.bas
./verprint ${GFX} ${MUSIC}
fbc -lang deprecated -g -v -exx -m game -d IS_GAME -d DATAFILES='"/usr/share/games/ohrrpgce"' \
  ${3} ${4} ${5} ${6} ${7} ${8} \
  game.bas bmod.bas bmodsubs.bas allmodex.bas menustuf.bas moresubs.bas yetmore.bas yetmore2.bas \
  compat.bas bam2mid.bas loading.bas common.bas browse.bas util.bas slices.bas gfxsubs.bas \
  lumpfile.bas gfx_${GFX}.bas music_${MUSIC}.bas ${EXTRA} \
&& mv game ohrrpgce-game
