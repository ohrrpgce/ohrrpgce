#!/bin/sh
GFX=${1}
MUSIC=${2}

if [ -z "${GFX}" ] ; then
  GFX=fb2
fi

if [ -z "${MUSIC}" ] ; then
  MUSIC=sdl
fi

cp -f fbcompat.bi compat.bi
cp -f fbcompat.bas compat.bas
./verprint
fbc -v -m game -d IS_GAME \
  game.bas bmod.bas bmodsubs.bas allmodex.bas menustuf.bas moresubs.bas yetmore.bas yetmore2.bas compat.bas bam2mid.bas gfx_"${GFX}".bas music_"${MUSIC}".bas gicon.rc \
&& mv game ohrrpgce-game
