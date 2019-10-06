#!/bin/sh

if [ ! -f "${FBCARM}" ] ; then
  echo "The FBCARM env variable should point to the fbc compiler for arm"
  exit 1
fi

if [ ! -d "${SDLANDROID}" ] ; then
  echo "The SDLANDROID env variable should point to the checked out copy of sdl-android ohrrpgce branch"
  exit 1
fi

SCRIPTDIR="${0%/*}"

cd "${SCRIPTDIR}"

svn cleanup
svn update | tee nightly-temp.txt || exit 1
UPDATE=`grep "Updated to revision" nightly-temp.txt`
rm nightly-temp.txt
if [ "$1" = "--force" ] ; then
  echo "Forcing a build, even if nothing has changed..."
  UPDATE="forced"
fi

if [ -z "$UPDATE" ] ; then
  echo No changes, no need to update nightly.
  exit
fi

scons fbc="${FBCARM}" release=1 android-source=1 game
cd "${SDLANDROID}"/project/jni/application

# Make sure we are on the ohrrpgce branch
git checkout ohrrpgce

# update the src link to point to the ohrrpgce project
rm src
ln -s ohrrpgce src

cd "${SDLANDROID}"
rm project/bin/MainActivity-debug.apk
./build.sh

if [ "$1" = "-noupload" ] ; then
  echo "skipping upload."
  exit
fi

if [ -f project/bin/MainActivity-debug.apk ] ; then
  scp -pr project/bin/MainActivity-debug.apk james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/ohrrpgce-game-android-debug.apk
fi
