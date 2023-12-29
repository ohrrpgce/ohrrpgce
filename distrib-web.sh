#!/bin/sh
#
# Build and package builds for linux

SCONS_ARGS="release=1"
FULLNAME=${1:-ohrrpgce-web-\{DATE\}-\{BRANCH\}}
PLAYERNAME=${2:-ohrrpgce-player-web-\{DATE\}-\{BRANCH\}}

SCRIPTDIR="${0%/*}"
SCRIPTDIR="$(realpath $SCRIPTDIR)"
cd "${SCRIPTDIR}"

echo "Building OHRRPGCE web binaries with emscripten"
scons $SCONS_ARGS target=js game custom || return 1

### Disabled because this would not be useful yet
#echo "Packaging web binary distribution of CUSTOM"
#./ohrpackage.py web full "distrib/$FULLNAME.zip" || return 1

echo "Prepare minimal web player zip"
./ohrpackage.py web player "distrib/$PLAYERNAME.zip" || return 1
