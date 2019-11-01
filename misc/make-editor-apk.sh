#!/bin/bash

# This script creates the experimental Android build of Custom. Maybe it will be
# merged into distrib-nightly-android.sh if it's useful enough.

ARCHARGS="arch=armeabi"

if [ -z "${FBCARM}" ] ; then
  echo "The FBCARM env variable should point to the fbc compiler for arm"
  exit 1
fi

if [ ! -d "${SDLANDROID}" ] ; then
  echo "The SDLANDROID env variable should point to the checked out copy of sdl-android ohrrpgce branch"
  exit 1
fi

PROJDIR="${SDLANDROID}"/project/jni/application

# Compile the source
scons fbc="${FBCARM}" release=1 android-source=1 "${ARCHARGS}" custom || exit 1

# Package data files
rm -f gamedata.zip
zip -r gamedata.zip data import ohrhelp
# Lets provide some example games too, for testing (note that you can't actually play these from this .apk)
zip gamedata.zip testgame/test.rpg testgame/a-star.rpg

mkdir -p "${PROJDIR}"/src/AndroidData
cp gamedata.zip "${PROJDIR}"/src/AndroidData

cd "${PROJDIR}"

# Make sure we are on the ohrrpgce branch
git checkout ohrrpgce

# update the src link to point to the ohrrpgce project
rm src
ln -s ohrrpgce src

cd "${SDLANDROID}"
rm project/bin/MainActivity-debug.apk
./build.sh
if [ ! -f project/bin/MainActivity-debug.apk ] ; then
  echo "Failed to build Android OHRRPGCE Custom apk for arch $CUR_ARCH"
  exit 1
fi

echo "Finished building $ARCHFLAGS"
