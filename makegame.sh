#!/bin/sh

# makegame.sh [graphicsbackend[+graphicsbackend...] [music_backend [extra_options]]]

# Usage example (gfx_sdl, gfx_fb, music_sdl profiling build):
#  makegame.sh sdl+fb sdl -d SCRIPT_PROFILING

GFX=${1}
MUSIC=${2}
EXTRA=

if [ -z "${GFX}" ] ; then
  GFX=sdl+fb
fi

if [ -z "${MUSIC}" ] ; then
  MUSIC=sdl
fi

for backend in `echo ${GFX} | tr "+" "\n"` ; do
  EXTRA="${EXTRA} gfx_${backend}.bas"
done
EXTRA="${EXTRA} music_${MUSIC}.bas"

gcc -c -O3 blit.c

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
fbc -lang deprecated -g -v -exx -mt -m game -d IS_GAME -d DATAFILES='"/usr/share/games/ohrrpgce"' \
  ${3} ${4} ${5} ${6} ${7} ${8} \
  game.bas bmod.bas bmodsubs.bas allmodex.bas menustuf.bas moresubs.bas yetmore.bas yetmore2.bas \
  compat.bas bam2mid.bas loading.bas common.bas browse.bas util.bas slices.bas blit.o \
  lumpfile.bas backends.bas ${EXTRA} \
&& mv game ohrrpgce-game
