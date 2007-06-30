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
fbc -lang deprecated -g -v -exx -m game -d IS_GAME \
  game.bas bmod.bas bmodsubs.bas allmodex.bas menustuf.bas moresubs.bas yetmore.bas yetmore2.bas compat.bas bam2mid.bas loading.bas common.bas util.bas gfx_"${GFX}".bas music_"${MUSIC}".bas ${EXTRA} \
&& mv game ohrrpgce-game
