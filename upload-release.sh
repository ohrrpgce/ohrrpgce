#!/bin/sh

TODAY=`date "+%Y-%m-%d"`
CODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
scp -p distrib/ohrrpgce-linux-x86-${TODAY}-${CODE}.tar.bz2 james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/archive/
scp -p distrib/ohrrpgce-player-linux-bin-minimal-${TODAY}-${CODE}.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/archive/

DEBTODAY=`date "+%Y.%m.%d"`
SVNREV=`svn info | grep "^Revision:" | sed -e "s/Revision: //"`
scp -p distrib/*_${DEBTODAY}.${CODE}-${SVNREV}_i386.deb james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/archive/debian/

scp -p whatsnew.txt james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/
