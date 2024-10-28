#!/bin/sh

SCRIPTDIR="${0%/*}"
cd "${SCRIPTDIR}"
cd ..

SCPDEST="james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce"

TODAY=`date "+%Y-%m-%d"`
CODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
BRANCH=`cat codename.txt | grep -v "^#" | head -2 | tail -1 | tr -d "\r"`

./ohrpackage.py unix source
cd ./ohrrpgce
zip -r ../distrib/ohrrpgce-source-${TODAY}-${BRANCH}.zip *
cd ..
rm -R ./ohrrpgce

scp -p distrib/ohrrpgce-source-${TODAY}-${BRANCH}.zip "${SCPDEST}"/archive/
