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
fbc -lang deprecated -g -v -exx -mt -m custom -d IS_CUSTOM -d DATAFILES='"/usr/share/games/ohrrpgce"' \
  ${3} ${4} ${5} ${6} ${7} ${8} \
  custom.bas customsubs.bas drawing.bas subs.bas subs2.bas subs3.bas mapsubs.bas flexmenu.bas \
  menus.bas allmodex.bas compat.bas common.bas browse.bas util.bas bam2mid.bas loading.bas \
  lumpfile.bas slices.bas sliceedit.bas reload.bas gfxsubs.bas \
  gfx_${GFX}.bas music_${MUSIC}.bas ${EXTRA} \
&& mv custom ohrrpgce-custom

