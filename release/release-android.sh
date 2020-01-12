#!/bin/sh

SCPDEST="james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce"

if [ ! -d "${SDLANDROID}" ] ; then
  echo "The SDLANDROID env variable should point to the checked out copy of sdl-android ohrrpgce branch"
  exit 1
fi

SCRIPTDIR="${0%/*}"
cd "${SCRIPTDIR}"
cd ..
RELDIR="$(pwd)"

CODENAME=$(grep -v "^#" codename.txt | head -1)
if [ "${CODENAME}" = "wip" ] ; then
  echo "Doing a release on the wip branch probably isn't what you meant to do"
  exit 1
fi

# First update the symlink
echo "Update ohrrpgce sdl-android symlink for ${CODENAME} release..."
cd "${SDLANDROID}"/project/jni/application/
rm ohrrpgce
ln -s ../../../../ohr/rel/"${CODENAME}"/android ohrrpgce
cd "${RELDIR}"

# Now build the apk
./distrib-nightly-android.sh -noupload -force

# Try to upload the result
cd "${SDLANDROID}"
if [ -f project/bin/MainActivity-debug.apk ] ; then
  TODAY=$(date "+%Y-%m-%d")
  scp -pr project/bin/MainActivity-debug.apk "${SCPDEST}"/archive/ohrrpgce-game-android-debug-"${TODAY}"-"${CODENAME}".apk
else
  echo "Nope, building that apk didn't work for some reason..."
fi

# Restore the wip symlink
echo "Restore ohrrpgce sdl-android symlink for wip"
cd project/jni/application
rm ohrrpgce
ln -s ../../../../ohr/wip/android ohrrpgce
