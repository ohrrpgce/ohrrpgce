#!/bin/sh
GFX=${1}
MUSIC=${2}

if [ -z "${GFX}" ] ; then
  GFX=fb
fi

if [ -z "${MUSIC}" ] ; then
  MUSIC=sdl
fi

cp -f fbcompat.bi compat.bi
cp -f fbcompat.bas compat.bas
./verprint
fbc -v -m custom -d IS_CUSTOM \
  custom.bas drawing.bas subs.bas subs2.bas subs3.bas mapsubs.bas flexmenu.bas menus.bas allmodex.bas compat.bas common.bas bam2mid.bas gfx_"${GFX}".bas music_"${MUSIC}".bas cicon.rc %1 %2 %3 %4 %5 %6 \
&& mv custom ohrrpgce-custom
