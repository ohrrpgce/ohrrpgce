#!/bin/sh

SCRIPTDIR="${0%/*}"
cd "${SCRIPTDIR}"
cd ..

OHRDIR="$(pwd)" "./docker/ohrrpgce-build-env-emscripten/emscr.sh" -c /src/ohr/distrib-web.sh

SCPDEST="james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce"

TODAY=`date "+%Y-%m-%d"`
CODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
BRANCH=`cat codename.txt | grep -v "^#" | head -2 | tail -1 | tr -d "\r"`
scp -p distrib/ohrrpgce-player-web-${TODAY}-${BRANCH}.zip "${SCPDEST}"/archive/
