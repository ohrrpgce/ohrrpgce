#!/bin/sh
# This is a helper and example script for optionally installing a standalone NDK toolchain,
# and some examples of compiling commandline FB or C programs for android.
# It is NOT used to create an .apk

echo "Please edit the config variables at the top of this file and the stuff to be compiled at the bottom of the file"
exit

########################## Config

# Android 5.0+ will only run PIE exes so requires -pie. Don't use -pie if targetting 4.0 or older, not supported.
#CFLAGS=-pie
CFLAGS=

# Build of fbc which can target android
FBC=fbc

#ARCH=x86
ARCH=arm
#ARCH=arm64

NDK=/opt/android-ndk-r8e
#NDK=/opt/android-ndk-r12b

# Comment out this to NOT install and use a standalone toolchain (which is a copy of part
# of the NDK, requires ~450MB!).
# Note: compiling OHR programs with scons won't work without a standalone toolchain.
STANDALONE=1

HOST=linux-$(uname -m)

# If STANDALONE, Android standalone toolchain will be placed here (stuff is copied from NDK directory)
TOOLCHAIN=$HOME/local/android-toolchain-r8e-api4-arm
#TOOLCHAIN=$HOME/local/android-toolchain-r8e-api8-x86
#TOOLCHAIN=$HOME/local/android-toolchain-r12b-api17-x86
# Otherwise, set to location of toolchain inside the NDK, e.g.
#TOOLCHAIN=$NDK/toolchains/arm-linux-androideabi-4.6/prebuilt/$HOST
#TOOLCHAIN=$NDK/toolchains/aarch64-linux-androideabi-4.9/prebuilt/$HOST
#TOOLCHAIN=$NDK/toolchains/x86-4.6/prebuilt/$HOST

echo $TOOLCHAIN

# For new NDKs, e.g. r12
OLDNDK=
# For older NDKs, e.g. r8
#OLDNDK=YES

########################## Install stand-alone toolchain if it hasn't been already

# Default to a really old API 4 (android 1.6)
if [ $ARCH = "arm" ]; then
    TARGET=arm-linux-androideabi
    API=4
    if [ ! $OLDNDK ]; then
        # For newer NDKs, eg r12b
        # r12b supports api 9 at a minimum
        API=9
    fi
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
        if [ $OLDNDK ]; then
            # For older NDKs, eg. r8
            $NDK/build/tools/make-standalone-toolchain.sh --arch=$ARCH --system=$HOST --platform=android-$API --install-dir=$TOOLCHAIN
        else
            $NDK/build/tools/make_standalone_toolchain.py --arch $ARCH --api $API --install-dir $TOOLCHAIN
        fi
    fi
fi

# SYSROOT is only used when not using a standalone toolchain
SYSROOT=$NDK/platforms/android-$API/arch-$ARCH

########################## Examples

PATH=$TOOLCHAIN/bin:$PATH

# An example of compiling testcases for android:
cd .. && scons -j6 fbc=$FBC target=$TARGET unlump relump vectortest reloadtest filetest utiltest rbtest reloadutil reload2xml


if [ ! $STANDALONE ]; then
    # Need to manually specify the sysroot, where the includes and libraries live
    CFLAGS="$CFLAGS --sysroot $SYSROOT"
fi

# An example of compiling a commandline FB program for android:
$FBC -target $TARGET -g test.bas -v

# If you want to try the examples below, not using scons, need to set these
export CC=$TOOLCHAIN/bin/$TARGET-gcc   # or export CC=clang
#export CXX=$TOOLCHAIN/bin/$TARGET-g++  # or export CXX=clang++
export AS=$TOOLCHAIN/bin/$TARGET-as  # or export CXX=clang++
export GCC=$CC
#export OBJDUMP=$TOOLCHAIN/bin/$TARGET-objdump

# Examples of compiling a commandline C program for android:
#$CC hello.c -o hello -g $CFLAGS -march=armv7-a
#$CC hello.c -static -o hello.o -g $CFLAGS -march=armv7-a

# An example of compiling a commandline FB program for android and manually linking it:
#BASFILE=test
#$FBC -target $TARGET -g $BASFILE.bas -m $BASFILE -r
#LIBDIR=$(dirname $(realpath $(which $FBC)))/../lib/freebasic/android-$ARCH
#$CC -g $LIBDIR/fbrt0pic.o $BASFILE.c -o $BASFILE -L$LIBDIR -lfbmtpic -llog $CFLAGS
