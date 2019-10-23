#!/bin/bash

FORCE=false
UPLOAD=true

# NOTE: "both" means compile two apks, one 32 bit and one 64 bit
# We do not yet have the ability to compile multi-arch
ARCH=both

POSITIONAL=()
while [[ $# -gt 0 ]]
do
  key="$1"
  
  case $key in
    -force|--force)
    FORCE=true
    shift # past argument
    ;;
    -noupload|--noupload)
    UPLOAD=false
    shift # past argument
    ;;
    -arch|--arch)
    ARCH="$2"
    shift # past argument
    shift # past value
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

# Check if a new nightly build is actually needed. Only if there are new changes
svn cleanup
svn update | tee nightly-temp.txt || exit 1
UPDATE=`grep "Updated to revision" nightly-temp.txt`
rm nightly-temp.txt
if [ "$FORCE" = "true" ] ; then
  echo "Forcing a build, even if nothing has changed..."
  UPDATE="forced"
fi

if [ -z "$UPDATE" ] ; then
  echo No changes, no need to update nightly.
  exit
fi

# Loop through the architectures we want to build
for CUR_ARCH in ${ARCHLIST[@]} ; do

case $CUR_ARCH in
  32)
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

if [ "$UPLOAD" = "false" ] ; then
  echo "skipping upload."
  continue
fi
scp -pr project/bin/MainActivity-debug.apk james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/ohrrpgce-game-android-debug"${ARCHSUFFIX}".apk

done
echo "Finished building arch $ARCH"
