#!/bin/sh

SCRIPTDIR="${0%/*}"
cd "${SCRIPTDIR}"
cd ..

# This is hardcoded for the paths on James's Mac build box
CC=clang GCC=/opt/local/bin/gcc-mp-4.7 EUDIR=~/misc/euphoria/ ARCH=x86_64 SDL=SDL2 ./distrib-mac.sh
CC=clang GCC=/opt/local/bin/gcc-mp-4.7 EUDIR=~/misc/eu32/ ARCH=i386 SDL=SDL ./distrib-mac.sh

SCPDEST="james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce"

TODAY=`date "+%Y-%m-%d"`
CODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
BRANCH=`cat codename.txt | grep -v "^#" | head -2 | tail -1 | tr -d "\r"`
scp -p distrib/OHRRPGCE-${TODAY}-${BRANCH}-*.dmg "${SCPDEST}"/archive/
scp -p distrib/ohrrpgce-player-mac-${TODAY}-${BRANCH}-*.tar.gz "${SCPDEST}"/archive/
