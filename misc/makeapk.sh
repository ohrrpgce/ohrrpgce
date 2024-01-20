#!/bin/bash

# This is the script that James uses to build Android apk files for specific
# games. It probably won't be useful to you directly, but might serve as a useful
# example. See instead the distrib-nightly-android.sh script if you are just
# interested in building a standalone Android OHRRPGCE game player.

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
    | grep -v -e "sdl-1.3-test" -e "_template"
  exit 1
fi

if [ ! -d "${PROJDIR}"/"${PROJECT}" ] ; then
  echo "${PROJDIR}/${PROJECT} does not exist"
  exit 1
fi

SCRIPTDIR="${0%/*}"
SCRIPTDIR="$(realpath $SCRIPTDIR)"

echo "=== Building OHRRPGCE apks for $PROJECT ==="
echo "PROJDIR=$PROJDIR"
echo "BRANCHSUFFIX=$BRANCHSUFFIX"
echo "BRANCHBASE=$BRANCHBASE"
echo "SCRIPTDIR=$SCRIPTDIR"

for CUR_ARCH in ${ARCHLIST[@]} ; do

case $CUR_ARCH in
  32)
    ARCHARGS="arch=armeabi"
    ARCHSUFFIX="_arm32"
    ArchVersionCode=0
    ;;
  64)
    ARCHARGS="arch=arm64"
    ARCHSUFFIX="_arm64"
    ArchVersionCode=1
    ;;
  *)
    echo "Invalid CUR_ARCH $CUR_ARCH"
    exit 1
    ;;
esac

echo "Will build apk for arch $CUR_ARCH"

cd "${SCRIPTDIR}"
cd ..

echo "Current working dir=$(pwd)"

echo "Removing stale files in ${SDLANDROID}/project/obj/local/"
rm -Rf "${SDLANDROID}"/project/obj/local/*

#echo "=== ENV before scons call ==="
#set
#echo "============================="

echo "Calling scons to compile ohrrpgce sources for android..."
scons fbc="${FBCARM}" release=1 android-source=1 "${ARCHARGS}" v=1 game || exit 1

# Use the "ohrrpgce" branch of sdl-android by default,
# but if ohrrpgce_gamename branch exists, use that instead.
cd "${PROJDIR}"
git checkout "${BRANCHBASE}"
git checkout "${BRANCHBASE}"_"${BRANCHSUFFIX}"

# Update src symlink
rm src
ln -s "${PROJECT}" src

# generate src/versioninfo.cfg based on src/gamespecific.cfg
BaseAppVersionCode=$(grep "^BaseAppVersionCode=" src/gamespecific.cfg | cut -d "=" -f 2)
PatchVersionCode=$(grep "^PatchVersionCode=" src/gamespecific.cfg | cut -d "=" -f 2)
AppVersionCode=$(expr "${BaseAppVersionCode}" '*' 1000 + "${ArchVersionCode}" '*' 100 + "${PatchVersionCode}" )
echo "AppVersionCode=${AppVersionCode}" > src/versioninfo.cfg

cd "${SDLANDROID}"
UNSIGNED="${SDLANDROID}"/project/bin/MainActivity-release-unsigned.apk
rm "${UNSIGNED}"
./build.sh release
if [ ! -f "${UNSIGNED}" ] ; then
  echo "Failed to build apk for game $PROJECT with arch $CUR_ARCH"
  exit 1
fi
cp "${UNSIGNED}" "${PROJDIR}"/"${PROJECT}"/"${PROJECT}"-release-unsigned"$ARCHSUFFIX".apk

done
echo "Finished building $PROJECT"
#src/ohrsign.sh
