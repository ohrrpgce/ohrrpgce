#!/bin/sh

# Scheduling this script to run automatically is equivalent to giving the other devs
# write access to your automatic build machine. Don't do it unless you trust them all.
# (which James fortunately does, and the build machine is reasonably sandboxed, so!)

MORE_ARGS=$*
UPLOAD_SERVER="james_paige@motherhamster.org"
UPLOAD_FOLDER="HamsterRepublic.com"
UPLOAD_DEST="$UPLOAD_SERVER:$UPLOAD_FOLDER"
TODAY=`date "+%Y-%m-%d"`

cd ~/src/nightly

if [ ! -d ohrrpgce ] ; then
  echo nightly snapshot not found, checking out from svn...
  svn checkout svn://gilgamesh.HamsterRepublic.com/ohrrpgce ./ohrrpgce
fi

cd ohrrpgce

svn update > ../nightly-temp.txt
UPDATE=`wc -l < ../nightly-temp.txt`
cat ../nightly-temp.txt
rm ../nightly-temp.txt

if [ ${UPDATE} -eq 1 ] ; then
  echo no changes, no need to update nightly.
  exit
fi

echo Now we go to build the Mac nightlies

svn cleanup
svn update

cd wip

./distrib-mac.sh ${MORE_ARGS}

if [ ! -f ohrrpgce-game -o ! -f ohrrpgce-custom ] ; then
  echo Aborting distrib-nightly script because distrib script failed
  exit 1
fi

mv distrib/OHRRPGCE-*-wip.dmg distrib/OHRRPGCE-wip.dmg
scp -p distrib/OHRRPGCE-wip.dmg $UPLOAD_DEST/ohrrpgce/nightly/
rm distrib/OHRRPGCE-wip.dmg

mv distrib/ohrrpgce-mac-minimal-*-wip.tar.gz distrib/ohrrpgce-mac-minimal.tar.gz
scp -p distrib/ohrrpgce-mac-minimal.tar.gz $UPLOAD_DEST/ohrrpgce/nightly/
rm distrib/ohrrpgce-mac-minimal.tar.gz

