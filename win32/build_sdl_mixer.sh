#!/bin/sh
#
# Script to cross-compile SDL_mixer.dll and SDL2_mixer.dll from Linux using mxe,
# and copy them (and other needed files) into the OHRRPGCE source tree.
# To use this script you will need wine and mxe, checked out to the fork at
# https://github.com/rversteegen/mxe.git. You don't need to have run 'make'
# after git cloning mxe yet.
#
# Note that there are many comments about the build configuration in
# src/sdl[2]_mixer_ohrrpgce.mk in mxe.

MXEDIR=${MXEDIR:-~/src/mxe}
OHRDIR=$(dirname "$(realpath -s "$0")")/..

build() {
    SCRIPT=$1
    SDL=$2
    SDL_PKG=$3

    cd $MXEDIR || exit 1

    STATIC_DIR=usr/i686-w64-mingw32.static
    SHARED_DIR=usr/i686-w64-mingw32.shared

    if [ ! -f $STATIC_DIR/lib/lib$SDL.la ] || grep -q "dlname=''" $STATIC_DIR/lib/lib$SDL.la; then
        echo "Missing/incorrect lib$SDL.la"

        # Currently disable using SDL from i686-w64-mingw32.shared because for some
        # reason ./configure doesn't define HAVE_MEMCMP, so SDL.h defines SDL_memcmp
        # resulting in a SDL_mixer.dll that only works with the mxe SDL.dll not with
        # an official SDL.dll built which lacks SDL_memcmp, arghh!!

        # if [ -f $SHARED_DIR/bin/$SDL.dll ]; then
        #     echo "Copying $SDL libs from i686-w64-mingw32.shared"
        #     cp -a $SHARED_DIR/bin/$SDL.dll $STATIC_DIR/bin/$SDL.dll  && \
        #     cp -a $SHARED_DIR/lib/lib$SDL.la $STATIC_DIR/lib/lib$SDL.la  && \
        #     cp -a $SHARED_DIR/lib/lib$SDL.dll.a $STATIC_DIR/lib/lib$SDL.dll.a  && \
        #     cp -ar $SHARED_DIR/include/$SDL $STATIC_DIR/include/ || exit 1
        # else
            echo "Downloading prebuilt SDL library"

            if [ ! -f pkg/${SDL_PKG} ]; then
                wget https://www.libsdl.org/release/${SDL_PKG} -O pkg/${SDL_PKG} || exit 1
            fi
            mkdir -p sdltmp && \
            tar xf pkg/${SDL_PKG} -C sdltmp && \
            cp -a sdltmp/SDL*/lib/*  $STATIC_DIR/lib/  && \
            cp -a sdltmp/SDL*/bin/* $STATIC_DIR/bin/  && \
            cp -ar sdltmp/SDL*/include/* $STATIC_DIR/include/  || exit 1
            rm -rf sdltmp
        # fi

        # Ensure lib$SDL.la contains correct dlname='../bin/$SDL.dll' and library_names='lib$SDL.dll.a'
        sed -i -e "s|^libdir=.*$|libdir='$(pwd)/usr/i686-w64-mingw32.static/lib'|" $STATIC_DIR/lib/lib$SDL.la

        if [ $SDL = SDL ]; then
            # ARGH, the libSDL.dll.a distributed in the official SDL 1.2.15 mingw build is faulty, it's
            # missing SDL_strdup although it's present in SDL.dll and declared by SDL.h, so SDL_mixer uses it.
            # mingw-w64 doesn't need .dll.a's, so delete it.
            rm $STATIC_DIR/lib/libSDL.dll.a
            sed -i -e "s|^library_names=.*$|library_names='../bin/SDL.dll'|" $STATIC_DIR/lib/libSDL.la
        fi
    fi

    make MXE_TARGETS=i686-w64-mingw32.static $SCRIPT || exit 1
    DLL=$STATIC_DIR/bin/${SDL}_mixer.dll
    if ! objdump -x $DLL | grep -q "$SDL\.dll"; then
        echo "$DLL seems to be statically linked to $SDL. This shouldn't have happened!"
        exit 1
    fi

    cp $DLL $OHRDIR
    cp $DLL $OHRDIR/win32/debug
    # The .dll.a file is needed by (old versions of, at least) mingw.org, but not by mingw-w64
    cp $STATIC_DIR/lib/lib${SDL}_mixer.dll.a $OHRDIR/win32

    cd $OHRDIR
    # Produces SDL_mixer.pdb and strips SDL_mixer.dll
    WINEDEBUG=fixme-all wine support/cv2pdb.exe ${SDL}_mixer.dll ${SDL}_mixer.dll || exit 1
    mv ${SDL}_mixer.pdb win32
}

build sdl_mixer_ohrrpgce SDL SDL-devel-1.2.15-mingw32.tar.gz
build sdl2_mixer_ohrrpgce SDL2 SDL2-devel-2.0.22-mingw.tar.gz
