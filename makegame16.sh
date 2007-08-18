#!/bin/sh
# This version of the compile script works with FreeBasic 0.16 and older
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

fbc verprint.bas
./verprint ${GFX} ${MUSIC}
fbc -g -v -m game -d IS_GAME \
  game.bas bmod.bas bmodsubs.bas allmodex.bas menustuf.bas moresubs.bas yetmore.bas yetmore2.bas compat.bas bam2mid.bas loading.bas common.bas browse.bas util.bas gfx_"${GFX}".bas music_"${MUSIC}".bas ${EXTRA} gicon.rc \
&& mv game ohrrpgce-game
