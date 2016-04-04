#!/bin/sh

#Not sure where I am supposed to upload utilities-mac.zip

TODAY=`date "+%Y-%m-%d"`
CODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
scp -p distrib/OHRRPGCE-${TODAY}-${CODE}.dmg james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/archive/
scp -p distrib/ohrrpgce-mac-minimal-${TODAY}-${CODE}.tar.gz james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/archive/

