#!/bin/sh

SCRIPTDIR="${0%/*}"
cd "${SCRIPTDIR}"
cd ..

svn info > svninfo.txt
OHRDIR="$(pwd)" "./docker/ohrrpgce-build-env-linux-x86_64/lin64.sh" -c /src/ohr/distrib-linux.sh

SCPDEST="james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce"

TODAY=`date -u "+%Y-%m-%d"`
CODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
BRANCH=`cat codename.txt | grep -v "^#" | head -2 | tail -1 | tr -d "\r"`
scp -p distrib/ohrrpgce-linux-${TODAY}-${BRANCH}-*.tar.bz2 "${SCPDEST}"/archive/
scp -p distrib/ohrrpgce-player-linux-${TODAY}-${BRANCH}-*.zip "${SCPDEST}"/archive/

DEBTODAY=`date -u "+%Y.%m.%d"`
SVNREV=`cat svninfo.txt | grep "^Revision:" | sed -e "s/Revision: //"`
scp -p distrib/*_${DEBTODAY}.${BRANCH}-${SVNREV}_amd64.deb "${SCPDEST}"/archive/debian/
