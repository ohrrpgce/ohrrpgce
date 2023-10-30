#!/bin/sh
# This is a helper and example script for optionally installing a standalone NDK toolchain,
# and some examples of compiling commandline FB or C programs for android.
# WARNING: For years now NDKs no longer support standalong toolchains,
# and this script isn't fully updated to support those NDKs!
# It is NOT used to create an .apk

set -e

echo "Please edit the config variables at the top of this file and the stuff to be compiled at the bottom of the file"
exit

DIR="$(dirname "$(readlink -e "$0")")"

########################## Config

# Android 5.0+ will only run PIE exes so requires -pie. Don't use -pie if targetting 4.0 or older, not supported.
#CFLAGS=-pie
CFLAGS=

# Build of fbc which can target android
FBC=fbc

#ARCH=x86
ARCH=arm
#ARCH=arm64

NDK=/opt/android-ndk-r12b

# Comment out this to NOT install and use a standalone toolchain (which is a copy of part
# of the NDK, requires ~450MB!).
# Note: compiling OHR programs with scons won't work without a standalone toolchain,
# unless it's a newer NDK that already acts like a standalone toolchain (not tested!)
# (TODO: follow the instructions at https://android.googlesource.com/platform/ndk/+/master/docs/UnifiedHeaders.md
# to get it working.)
# NDK standalone toolchains are obsolete beginning with NDK r19, and no longer needed.
STANDALONE=1

HOST=linux-$(uname -m)

# If STANDALONE, Android standalone toolchain will be placed here (stuff is copied from NDK directory)
#TOOLCHAIN=$HOME/local/android-toolchain-r12b-api17-x86
TOOLCHAIN=$HOME/local/android-toolchain-r12b-api9-arm
# Otherwise, set to location of toolchain inside the NDK, e.g.
#TOOLCHAIN=$NDK/toolchains/arm-linux-androideabi-4.6/prebuilt/$HOST
#TOOLCHAIN=$NDK/toolchains/aarch64-linux-androideabi-4.9/prebuilt/$HOST
#TOOLCHAIN=$NDK/toolchains/x86-4.6/prebuilt/$HOST


########################## Install stand-alone toolchain if it hasn't been already

# Default to  API 9 (android 2.3) which is minimum supported by eg NDK r12b
# FIXME: Newer NDKS support API 16 (android 4.1) at a minimum
if [ $ARCH = "arm" ]; then
    TARGET=arm-linux-androideabi
    API=9
elif [ $ARCH = "arm64" ]; then
    TARGET=aarch64-linux-androideabi
    API=21  # Introduced aarch64
elif [ $ARCH = "x86" ]; then
    TARGET=i686-linux-android
    API=9   # Introduced x86
else
    echo "Bad ARCH value"
    exit 1
fi

if [ $STANDALONE ]; then
    if [ ! -d $TOOLCHAIN ]; then
        # Defaults to armeabi
        "$NDK/build/tools/make_standalone_toolchain.py" --arch $ARCH --api $API --install-dir "$TOOLCHAIN"
    fi

    PATH=$TOOLCHAIN/bin:$PATH
    echo "TOOLCHAIN = $TOOLCHAIN"

else
    # SYSROOT is only used when not using a standalone toolchain

    SYSROOT="$NDK/platforms/android-$API/arch-$ARCH"
    # Newer NDK (r14+?), -isystem arg for unified headers
    ISYSTEM="$NDK/sysroot/usr/include/$TARGET"

    if [ ! -d $SYSROOT ]; then
        echo "$SYSROOT doesn't exist, please edit config vars in this file."
        exit 1
    fi

    NDK_BIN=$(dirname $("$NDK/ndk-which" gcc))
    PATH="$NDK_BIN:$PATH"
fi



########################## Examples


# An example of compiling testcases for android:
cd .. && scons -j6 fbc=$FBC target=$TARGET unlump relump vectortest reloadtest filetest utiltest rbtest reloadutil reload2xml
cd android

if [ ! $STANDALONE ]; then
    # Need to manually specify the sysroot, where the includes and libraries live
    CFLAGS="$CFLAGS --sysroot $SYSROOT"
fi

# An example of compiling a commandline FB program for android:
$FBC -target $TARGET -sysroot "$SYSROOT" -g test.bas -v

# If you want to try the examples below, not using scons, need to set these
if [ $STANDALONE ]; then
    export CC=$TOOLCHAIN/bin/$TARGET-gcc   # or export CC=clang
    #export CXX=$TOOLCHAIN/bin/$TARGET-g++  # or export CXX=clang++
    export AS=$TOOLCHAIN/bin/$TARGET-as  # or export CXX=clang++
    #export OBJDUMP=$TOOLCHAIN/bin/$TARGET-objdump
else
    export CC=$("$NDK/ndk-which" gcc)
    export AS=$("$NDK/ndk-which" as)
fi
export GCC=$CC

# Examples of compiling a commandline C program for android:
#$CC hello.c -o hello -g $CFLAGS -march=armv7-a
#$CC hello.c -static -o hello.o -g $CFLAGS -march=armv7-a

#$CC jnitest.c -o hello -g $CFLAGS -march=armv7-a

# An example of compiling a commandline FB program for android and manually linking it:
#BASFILE=test
#$FBC -target $TARGET -g $BASFILE.bas -m $BASFILE -r
#LIBDIR=$(dirname $(realpath $(which $FBC)))/../lib/freebasic/android-$ARCH
#$CC -g $LIBDIR/fbrt0pic.o $BASFILE.c -o $BASFILE -L$LIBDIR -lfbmtpic -llog $CFLAGS
