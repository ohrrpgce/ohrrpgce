#!/bin/bash

# This script returns 0 on a successful build, 1 on error.
# Output is written into the distrib folder.

CHROMEBOOK=
# NOTE: "both" means compile two apks, one 32 bit and one 64 bit
# We do not yet have the ability to compile multi-arch
ARCH=both

POSITIONAL=()
while [[ $# -gt 0 ]]
do
  key="$1"
  
  case $key in
    -arch|--arch)
    ARCH="$2"
    shift # past argument
    shift # past value
    ;;
    -chromebook|--chromebook)
    # Create a build with no onscreen buttons, for Chromebooks and other
    # devices with keyboards.
    CHROMEBOOK=-chromebook
    # This affects android/AndroidAppSettings.cfg, called by build.sh
    export HASKEYBOARD=yes
    shift # past argument
    ;;
    *)    # unknown option
    POSITIONAL+=("$1") # save it in an array for later
    shift # past argument
    ;;
  esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters

case $ARCH in
  32)
    ARCHLIST=( 32 )
    ;;
  64)
    ARCHLIST=( 64 )
    ;;
  both)
    ARCHLIST=( 32 64 )
    ;;
  *)
    echo "Valid values for the --arch argument are '32' '64' and 'both'"
    exit 1
    ;;
esac

if [ ! -f "${FBCARM}" ] ; then
  echo "The FBCARM env variable should point to the fbc compiler for arm"
  exit 1
fi

if [ ! -d "${SDLANDROID}" ] ; then
  echo "The SDLANDROID env variable should point to the checked out copy of sdl-android ohrrpgce branch"
  exit 1
fi

SCRIPTDIR="${0%/*}"
SCRIPTDIR="$(realpath $SCRIPTDIR)"

cd "${SCRIPTDIR}"

# Loop through the architectures we want to build
for CUR_ARCH in ${ARCHLIST[@]} ; do

case $CUR_ARCH in
  32)
    # TODO: is it time to upgrade to armv7-a, which has faster floating point?
    # At some point Android dropped armeabi
    ARCHARGS="arch=armeabi"
    ARCHSUFFIX=""
    ;;
  64)
    ARCHARGS="arch=arm64"
    ARCHSUFFIX="_arm64"
    ;;
  *)
    echo "Invalid CUR_ARCH $CUR_ARCH"
    exit 1
    ;;
esac

cd "${SCRIPTDIR}"

# Cleanup old files
rm -Rf "${SDLANDROID}"/project/obj/local/*

# Work around an obscure scons error
touch .sconsign.dblite

# Compile the source
scons fbc="${FBCARM}" release=1 android-source=1 "${ARCHARGS}" game || exit 1
cd "${SDLANDROID}"/project/jni/application

# Make sure we are on the ohrrpgce branch
git checkout ohrrpgce

# update the src link to point to the ohrrpgce project
rm src
ln -s ohrrpgce src

cd "${SDLANDROID}"
rm project/bin/MainActivity-debug.apk
./build.sh
if [ ! -f project/bin/MainActivity-debug.apk ] ; then
  echo "Failed to build Android apk for arch $CUR_ARCH"
  exit 1
fi

cp -pr project/bin/MainActivity-debug.apk "${SCRIPTDIR}"/distrib/ohrrpgce-game-android"${CHROMEBOOK}"-debug"${ARCHSUFFIX}".apk

done
echo "Finished building arch $ARCH $CHROMEBOOK"
