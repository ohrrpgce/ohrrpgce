# This is a helper script for invoking scons in order to compile command-line Android programs.
# (There are also some examples for compiling C/C++ using the toolchain)
# It is NOT used to create an .apk

echo "Please edit the variables at the top of this file"
exit

# Build of fbc which targets android
FBC=~/local/fbc-0.90-android/bin/fbc
# defaults to 32 bit toolchain, so need to specify the host on x86-64
HOST=linux-x86_64
NDK=/opt/android-ndk-r8e
# Android standalone toolchain will be placed here (stuff is copied from NDK directory)
TOOLCHAIN=/tmp/android-toolchain
#SYSROOT=$NDK/platforms/android-8/arch-arm


if [ ! -d $TOOLCHAIN ]; then
    # android-4 = 1.6
    # Defaults to armeabi
    $NDK/build/tools/make-standalone-toolchain.sh --system=$HOST --platform=android-4 --install-dir=$TOOLCHAIN
fi

#PATH=$TOOLCHAIN/bin:$PATH
export CC=$TOOLCHAIN/bin/arm-linux-androideabi-gcc   # or export CC=clang
export CXX=$TOOLCHAIN/bin/arm-linux-androideabi-g++  # or export CXX=clang++
export AS=$TOOLCHAIN/bin/arm-linux-androideabi-as  # or export CXX=clang++

LIBDIR=$(dirname $FBC)/../lib/arm-linux-androideabi-freebasic/


cd .. && scons fbc=$FBC android=1 debug=1 unlump relump reload vectortest


# An example of compiling a commandline program for android:
#$CC hello.c -o hello
# Compiling a FB commandline program for android (fbrt0.o should be first):
# (haven't tried linking with fbc)
#$FBC -gen gcc -r test.bas -m test
#$CXX testmodule.cpp -c -o testmod.o
#$CXX $LIBDIR/fbrt0.o test.o testmod.o -o test -L$LIBDIR -lfbmt
