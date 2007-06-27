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
  make CXXFLAGS=-g
  cd ..
  #EXTRA="${EXTRA} -p audwrap -l audwrap"
fi

fbc -lang deprecated verprint.bas
./verprint ${GFX} ${MUSIC}
fbc -lang deprecated -g -v -exx -m custom -d IS_CUSTOM -d EXXERROR \
  custom.bas drawing.bas subs.bas subs2.bas subs3.bas mapsubs.bas flexmenu.bas menus.bas allmodex.bas compat.bas common.bas util.bas bam2mid.bas loading.bas gfx_"${GFX}".bas music_"${MUSIC}".bas ${EXTRA} \
&& mv custom ohrrpgce-custom
