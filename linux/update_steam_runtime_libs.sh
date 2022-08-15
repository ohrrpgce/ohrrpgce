#!/bin/sh
#
# Script to copy libSDL2.so for x86 and x86_64 Linux into the OHRRPGCE source
# tree from a steam-runtime installation, such as that installed by the Steam
# Linux client.

set -e

if [ -d "$1" ]; then
    STEAM_RUNTIME="$1"
elif [ -d "$STEAM_RUNTIME" ]; then
    echo OK
else
    STEAM_RUNTIME="$HOME/.local/share/Steam/ubuntu12_32/steam-runtime"
    if [ ! -d "$STEAM_RUNTIME" ]; then
        echo "Usage: $0 path_to_steam_runtime"
        echo "The runtime ought to be inside a Steam/ubuntu12_32 directory."
        exit 1
    fi
fi

LIB32="$STEAM_RUNTIME/usr/lib/i386-linux-gnu"
LIB64="$STEAM_RUNTIME/usr/lib/x86_64-linux-gnu"

if [ ! -d "$LIB32" -o ! -d "$LIB64" ]; then
    echo "$STEAM_RUNTIME doesn't appear to be a steam-runtime"
    exit 1
fi

echo "STEAM_RUNTIME = $STEAM_RUNTIME"

OHRDIR="$(dirname "$(readlink -e "$0")")/.."
cd "$OHRDIR/linux"

mkdir -p x86 x86_64

copylib() {
    DESTDIR="$1"
    SRCDIR="$2"
    LIB="$3"
    SRC="$SRCDIR/$LIB"
    echo $SRC.?
    cp -aL "$SRC".? "$DESTDIR"
    echo "$LIB ->" $(readlink -v "$SRC".?)
    #objdump -x "$SRC".? | grep NEEDED
}

# Don't copy libSDL2_mixer-2.0.so.0, we use a custom build

echo "-- x86 libs --"
for lib in libSDL2-2.0.so ; do
  copylib x86 "$LIB32" $lib
done

echo "-- x86_64 libs --"
for lib in libSDL2-2.0.so; do
  copylib x86_64 "$LIB64" $lib
done

# SDL2 is a special case, has this alias
ln -sf "libSDL2-2.0.so.0" "x86/libSDL2.so"
ln -sf "libSDL2-2.0.so.0" "x86_64/libSDL2.so"
