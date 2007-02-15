#!/bin/sh
GFX=${1}
MUSIC=${2}

if [ -z "${GFX}" ] ; then
  GFX=fb
fi

if [ -z "${MUSIC}" ] ; then
  MUSIC=sdl
fi

fbc verprint.bas
./verprint
fbc -g -v -m custom -d IS_CUSTOM \
  custom.bas drawing.bas subs.bas subs2.bas subs3.bas mapsubs.bas flexmenu.bas menus.bas allmodex.bas compat.bas common.bas util.bas bam2mid.bas loading.bas gfx_"${GFX}".bas music_"${MUSIC}".bas cicon.rc %1 %2 %3 %4 %5 %6 \
&& mv custom ohrrpgce-custom
