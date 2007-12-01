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
  make CXXFLAGS=-g
  cd ..
  #EXTRA="${EXTRA} -p audwrap -l audwrap"
fi

fbc verprint.bas
./verprint ${GFX} ${MUSIC}
fbc -g -v -m custom -d IS_CUSTOM \
  custom.bas customsubs.bas drawing.bas subs.bas subs2.bas subs3.bas mapsubs.bas flexmenu.bas menus.bas allmodex.bas compat.bas common.bas browse.bas util.bas bam2mid.bas loading.bas gfx_"${GFX}".bas music_"${MUSIC}".bas ${EXTRA} cicon.rc %1 %2 %3 %4 %5 %6 \
&& mv custom ohrrpgce-custom
