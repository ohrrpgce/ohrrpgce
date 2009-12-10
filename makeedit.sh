#!/bin/sh
GFX=${1}
MUSIC=${2}
EXTRA=

if [ -z "${GFX}" ] ; then
  GFX=sdl
fi

if [ -z "${MUSIC}" ] ; then
  MUSIC=sdl
fi

if [ "${MUSIC}" = "native" ] ; then
  cd audwrap
  make
  cd ..
  #./libaudwrap.a is built for windows!
  EXTRA="${EXTRA} -Wl audwrap/libaudwrap.a -l audiere -l vorbisfile -l vorbis -l ogg"
fi

if [ "${MUSIC}" = "sdl" ] ; then
  EXTRA="${EXTRA} sdl_lumprwops.bas"
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

