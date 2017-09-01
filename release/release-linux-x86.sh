#!/bin/sh

SCRIPTDIR="${0%/*}"
cd "${SCRIPTDIR}"
cd ..

OHR_SKIP_X86_64="Yes" ./distrib-nightly-linux.sh

SCPDEST="james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce"

TODAY=`date "+%Y-%m-%d"`
CODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
scp -p distrib/ohrrpgce-linux-${TODAY}-${CODE}-*.tar.bz2 "${SCPDEST}"/archive/
scp -p distrib/ohrrpgce-player-linux-bin-minimal-${TODAY}-${CODE}-*.zip "${SCPDEST}"/archive/

scp -p whatsnew.txt "${SCPDEST}"/
