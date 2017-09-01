#!/bin/sh

SCPDEST="james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce"

if [ ! -d "${SDLANDROID}" ] ; then
  echo "The SDLANDROID env variable should point to the checked out copy of sdl-android ohrrpgce branch"
  exit 1
fi

SCRIPTDIR="${0%/*}"
cd "${SCRIPTDIR}"
cd ..

CODENAME=$(grep -v "^#" codename.txt | head -1)
if [ "${CODENAME}" = "wip" ] ; then
  echo "Doing a release on the wip branch probably isn't what you meant to do"
  exit 1
fi

exit 1

# First update the symlink
pushd "${SDLANDROID}"/project/jni/application/
mv ohrrpgce ohrrpgce.orig_link
ln -s ../../../../ohr/rel/"{$CODENAME}"/android ohrrpgce
popd

# Now build the apk
./distrib-nightly-android.sh -noupload

# Try to upload the result
pushd "${SDLANDROID}"
if [ -f project/bin/MainActivity-debug.apk ] ; then
  TODAY=$(date "+%Y-%m-%d")
  scp -pr project/bin/MainActivity-debug.apk "${SCPDEST}"/archive/ohrrpgce-game-android-debug-"${TODAY}"-"${CODENAME}".apk
else
  echo "Nope, building that apk didn't work for some reason..."
fi

# Restore the wip symlink
cd project/jni/application
rm ohrrpgce
mv ohrrpgce.orig_link ohrrpgce
