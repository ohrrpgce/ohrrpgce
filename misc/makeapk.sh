#!/bin/sh

# This is the script that James uses to build Android apk files for specific
# games. It probably won't be useful to you directly, but might serve as a useful
# example. See instead the distrib-nightly-android.sh script if you are just
# interested in building a standalone Android OHRRPGCE game player.

if [ ! -f "${FBCARM}" ] ; then
  echo "The FBCARM env variable should point to the fbc compiler for arm"
  exit 1
fi

if [ ! -d "${SDLANDROID}" ] ; then
  echo "The SDLANDROID env variable should point to the checked out copy of sdl-android ohrrpgce branch"
  exit 1
fi

PROJECT="${1}"
PROJDIR="${SDLANDROID}"/project/jni/application
BRANCHSUFFIX="${2}"
BRANCHBASE="ohrrpgce"

if [ -z "${BRANCHSUFFIX}" ] ; then
  BRANCHSUFFIX="${PROJECT}"
fi

if [ -z "${PROJECT}" ] ; then
  echo "Specify a project name on the command line:"
  ls "${PROJDIR}"/*/AndroidAppSettings.cfg \
    | rev \
    | cut -f 2 -d "/" \
    | rev \
    | grep -v -e "sdl-1.3-test" -e "_template" \
    | column
  exit 1
fi

if [ ! -d "${PROJDIR}"/"${PROJECT}" ] ; then
  echo "${PROJDIR}/${PROJECT} does not exist"
  exit 1
fi

SCRIPTDIR="${0%/*}"
cd "${SCRIPTDIR}"
cd ..

scons fbc="${FBCARM}" release=1 android-source=1 game
cd "${PROJDIR}"
git checkout "${BRANCHBASE}"
git checkout "${BRANCHBASE}"_"${BRANCHSUFFIX}"
rm src
ln -s "${PROJECT}" src
cd "${SDLANDROID}"
rm project/bin/MainActivity-debug.apk
./build.sh release
#src/ohrsign.sh
