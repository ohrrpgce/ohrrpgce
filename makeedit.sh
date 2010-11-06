#!/bin/sh

# makeedit.sh [graphicsbackend[+graphicsbackend...] [music_backend [extra_options]]]

# Usage example (gfx_sdl, gfx_fb, music_sdl profiling build):
#  makeedit.sh sdl+fb sdl -d SCRIPT_PROFILING

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
  if [ "${backend}" = "sdlpp" ] ; then
    cd gfx_sdl/source
    make old
    cd ../..
    # EXTRA="${EXTRA} -Wl gfx_sdl/source/libgfx_sdl++_old.a"
  else
    EXTRA="${EXTRA} gfx_${backend}.bas"
  fi
done
EXTRA="${EXTRA} music_${MUSIC}.bas"

gcc -c -g -O3 blit.c
gcc -c -g -O3 base64.c --std=c99

if [ "${MUSIC}" = "native" ] ; then
  cd audwrap
  make
  cd ..
  EXTRA="${EXTRA} -Wl audwrap/libaudwrap.a -l audiere -l vorbisfile -l vorbis -l ogg"
fi

if [ "${MUSIC}" = "sdl" ] ; then
  EXTRA="${EXTRA} sdl_lumprwops.bas"
fi

if [ `uname` = "FreeBSD" ] ; then
  # apparent FB bug
  EXTRA="${EXTRA} -l gcc_s"
fi

fbc -lang deprecated verprint.bas
./verprint ${GFX} ${MUSIC}
fbc -lang deprecated -g -v -exx -mt -m custom -d IS_CUSTOM -d DATAFILES='"/usr/share/games/ohrrpgce"' \
  ${3} ${4} ${5} ${6} ${7} ${8} \
  custom.bas customsubs.bas drawing.bas subs.bas subs2.bas mapsubs.bas flexmenu.bas \
  menus.bas allmodex.bas compat.bas common.bas browse.bas util.bas bam2mid.bas loading.bas \
  lumpfile.bas slices.bas sliceedit.bas reload.bas reloadext.bas reloadedit.bas editedit.bas editrunner.bas backends.bas \
  blit.o base64.o ${EXTRA} \
&& mv custom ohrrpgce-custom

