#!/bin/sh
#
# Script to compile libxmp-lite.so and libSDL2_mixer.so for x86 and x86_64 linux
# and copy them into the OHRRPGCE source tree.
#
# To create portable libraries, an older toolchain should be used.
#
# These scripts mirror src/sdl2_mixer_ohrrpgce.mk and src/libxmp-lite.mk from the
# mxe fork https://github.com/rversteegen/mxe.git used by win32/build_sdl_mixer.sh
# to make it easier to
#
# Additionally, mxe's download cache directory can be shared by this script if
# MXEDIR is set.

set -e

OHRDIR="$(dirname "$(readlink -e "$0")")/.."
MXEDIR="${MXEDIR:-~/src/mxe}"
TEMPDIR="$(pwd)/tmpbuild"

MAKE=make
SED=sed

if [ -z "${DOWNLOAD_DIR}" ]; then
    if [ -d "$MXEDIR" ]; then
        # Reuse mxe's download cache, which is also used by win32/build_sdl_mixer.sh
        DOWNLOAD_DIR=$MXEDIR/pkg
    else
        DOWNLOAD_DIR=download_cache
    fi
fi
echo "*    Placing downloaded archives in ${DOWNLOAD_DIR}"

LIBDIR32="${OHRDIR}/linux/x86"
LIBDIR64="${OHRDIR}/linux/x86_64"

if [ ! -e "$LIBDIR32/libSDL2.so" ] || [ ! -e "$LIBDIR64/libSDL2.so" ] ; then
    echo "*    Warning: update_steam_runtime_libs.sh hasn't been run, will compile against host's libSDL2"
fi
mkdir -p "$LIBDIR32" "$LIBDIR64"

pkg-config --exists sdl2 || {
    echo "SDL2 needs to be installed on the host system (and should be relatively recent), for its headers."
    exit 1
}
pkg-config --exists fluidsynth || {
    echo "fluidsynth needs to be installed on the host system."
    exit 1
}


# Downloads and sets $ARCHIVE
download_pkg() {
    declare -n PKG=$1
    ARCHIVE="${DOWNLOAD_DIR}/${PKG[FILE]}"
    if [ ! -f "$ARCHIVE" ]; then
        wget "${PKG[URL]}" -O "$ARCHIVE"
    fi
    SUM=$(sha256sum "$ARCHIVE" | cut -d' ' -f1)
    if [ $SUM != "${PKG[CHECKSUM]}" ]; then
        echo "Download failed, checksum mismatch"
        rm "$ARCHIVE"
        exit 1
    fi
}

# Downloads, extracts, and sets $SOURCE_DIR and $BUILD_DIR
extract_pkg() {
    declare -n PKG=$1

    download_pkg $1
    #rm -rf "$TEMPDIR"
    mkdir -p "$TEMPDIR"
    SOURCE_DIR="$TEMPDIR/${PKG[SUBDIR]}"
    rm -rf "$SOURCE_DIR"
    BUILD_DIR="$TEMPDIR/build"

    echo
    echo "*    Extract $ARCHIVE to $TEMPDIR"
    case "${PKG[FILE]}" in
        *.tar.gz )
            tar xf "$ARCHIVE" -C "$TEMPDIR"
            ;;
        *.zip )
            unzip -q "$ARCHIVE" -d "$TEMPDIR"
    esac
}

# Expects as argument the name of a function name which compiles and installs a .so
x86_compile() {
    BUILD_FUNC=$1

    LIBDIR="$LIBDIR32"
    export CFLAGS="-m32 -O2"
    export LDFLAGS=-m32
    echo
    echo
    $BUILD_FUNC
}

x86_64_compile() {
    BUILD_FUNC=$1

    LIBDIR="$LIBDIR64"
    export CFLAGS=-O2
    export LDFLAGS=
    echo
    echo
    $BUILD_FUNC
}


##### Based on libxmp-lite.mk #####

declare -A libxmp_lite
declare -n PKG=libxmp_lite
PKG[WEBSITE]=https://xmp.sourceforge.net/
PKG[DESCR]="The Extended Module Player library, lite version"
PKG[VERSION]=4.5.1
PKG[GIT_REV]=a4b0b3c07bb682cd226bfaac8da570f9c0336dbe
PKG[CHECKSUM]=5c1b2848868e2511f390a03baf4419ac506461dcc2e94b0b898b16189f9c82e2
#PKG[SUBDIR]=libxmp-${PKG[VERSION]}
#PKG[FILE]=libxmp-${PKG[VERSION]}.zip
PKG[SUBDIR]=libxmp-${PKG[GIT_REV]}
PKG[FILE]=libxmp-${PKG[GIT_REV]}.zip
PKG[URL]=https://github.com/libxmp/libxmp/archive/${PKG[GIT_REV]}.zip

build_libxmp() {
    BUILD_DIR="$TEMPDIR/libxmp-lite"
    rm -rf "${BUILD_DIR}"

    # Work around bug
    ${SED} -i "s/configure configure.ac/configure.ac/" "${SOURCE_DIR}/Makefile.lite"

    #cd '${SOURCE_DIR}' && ${MAKE} -f Makefile.lite
    # Avoid redundant configure and build. Creates libxmp-lite-stagedir and a tar ball
    cd "${SOURCE_DIR}" && ${MAKE} -f Makefile.lite dist-prepare dist-subdirs
    mv -T "${SOURCE_DIR}"/libxmp-lite-stagedir "${BUILD_DIR}"

    echo CFLAGS= $CFLAGS
    cd "${BUILD_DIR}" && autoconf && ./configure

    ${MAKE} -C "${BUILD_DIR}" -j6

    LIBNAME=libxmp-lite.so.4
    cp "${BUILD_DIR}/lib/$LIBNAME" "$LIBDIR"
    ln -sf $LIBNAME "$LIBDIR"/libxmp-lite.so
}


##### Based on sdl_mixer_ohrrpgce.mk #####

declare -A sdl2_mixer
declare -n PKG=sdl2_mixer
PKG[WEBSITE]=https://www.libsdl.org/projects/SDL_mixer/
PKG[DESCR]=SDL2_mixer
PKG[VERSION]=2.6.1
PKG[CHECKSUM]=8c7193d3b6ae8d44f4436e53b26735b02f6cceb70fb1a3ae3f44ebd0c965e0a6
PKG[SUBDIR]=SDL_mixer-release-${PKG[VERSION]}
PKG[FILE]=SDL_mixer-release-${PKG[VERSION]}.tar.gz
PKG[URL]=https://github.com/libsdl-org/SDL_mixer/archive/refs/tags/release-${PKG[VERSION]}.tar.gz

build_sdl2_mixer() {
    # Might as well allow runtime linked libopusfile although we don't use it, yet (only
    # adds 1kB)... so don't complain if it isn't installed.

    # Don't need to have libxmp-lite installed because we have a copy.
    # By passing LDFLAGS to configure, it will look in there for libxmp-lite.so.* for runtime linking.
    # (passing only XMP_LIBS will not make runtime linking work, nor will having libxmp installed).

    # For me, "sdl2-config --libs sdl2" includes an /usr/lib64 rpath, which seems
    # dangerous although apparently -Wl,--enable-new-dtags means it's added as a RUNPATH
    # rather than RPATH, so should have a lower priority than LD_LIBRARY_PATH... but only
    # on newer Linux systems that support RUNPATH? So override it with SDL_LIBS=-lSDL2.
    # (FWIW steam-runtime libSDL2_mixer.so has no RUNPATH)
    # Need to pass SDL_CFLAGS too otherwise SDL_LIBS is ignored.

    rm -rf "${BUILD_DIR}"
    mkdir "${BUILD_DIR}"

    cd "$BUILD_DIR" && "$SOURCE_DIR"/configure \
        --enable-shared \
        --disable-static \
        --disable-sdltest \
        --enable-music-mod \
        --disable-music-mod-modplug \
        --enable-music-mod-xmp \
        --enable-music-mod-xmp-lite \
        --enable-music-mod-xmp-shared \
        --enable-music-ogg \
        --enable-music-ogg-stb \
        --disable-music-flac \
        --enable-music-mp3 \
        --enable-music-mp3-drmp3 \
        --enable-music-opus \
        --enable-music-opus-shared \
        LDFLAGS="-L$LIBDIR $LDFLAGS" \
        SDL_CFLAGS="$(sdl2-config --cflags)" \
        SDL_LIBS="-lSDL2" \
        XMP_CFLAGS="-I$TEMPDIR/libxmp-lite/include"
        #XMP_LIBS="libxmp-lite.so.4"

    ${MAKE} -C "$BUILD_DIR" -j6

    LIBNAME=libSDL2_mixer-2.0.so.0
    cp "${BUILD_DIR}/build/.libs/$LIBNAME" "$LIBDIR"
    ln -sf $LIBNAME "$LIBDIR"/libSDL2_mixer.so
}



extract_pkg libxmp_lite
x86_compile build_libxmp
x86_64_compile build_libxmp

extract_pkg sdl2_mixer
x86_compile build_sdl2_mixer
x86_64_compile build_sdl2_mixer

echo
cd "$OHRDIR"
ls -l "$LIBDIR32"/* "$LIBDIR64"/*
misc/linux_portability_check.py "$LIBDIR32"/*.so "$LIBDIR64"/*.so

rm -rf tmpbuild
