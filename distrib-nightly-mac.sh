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
  svn checkout https://rpg.hamsterrepublic.com/source ./ohrrpgce || exit 1
fi

cd ohrrpgce

svn cleanup
# Plotdict gets modified by update-html.sh, remove any modifications or conflicts
svn resolve --accept theirs-full --recursive docs
svn revert --recursive docs

svn update --trust-server-cert-failures=unknown-ca --non-interactive | tee ../nightly-temp.txt || exit 1
UPDATE=`grep "Updated to revision" ../nightly-temp.txt`
rm ../nightly-temp.txt

if [ -z "$UPDATE" ] ; then
  echo no changes, no need to update nightly.
  exit
fi

echo Now we go to build the Mac nightlies

svn update --trust-server-cert-failures=unknown-ca --non-interactive || exit 1

cd wip

build_package() {
  # distrib-mac.sh reads these envvars
  export ARCH=$1
  export SDL=$2

  if [ $ARCH = "x86_64" ]; then
    SUFFIX=-x86_64
  else
    SUFFIX=-x86
  fi
  echo "SUFFIX=${SUFFIX}"

  ./distrib-mac.sh ${MORE_ARGS} || return
  echo "Done running distrib-mac.sh"

  NEWESTMATCH=$(ls -1 distrib/OHRRPGCE-*-wip$SUFFIX.dmg | tail -1)
  mv -v "$NEWESTMATCH" distrib/OHRRPGCE-wip$SUFFIX.dmg
  scp -p distrib/OHRRPGCE-wip$SUFFIX.dmg $UPLOAD_DEST/ohrrpgce/nightly/
  rm distrib/OHRRPGCE-wip$SUFFIX.dmg

  NEWESTMATCH=$(ls -1 distrib/ohrrpgce-player-mac-*-wip$SUFFIX.tar.gz | tail -1)
  mv -v "$NEWESTMATCH" distrib/ohrrpgce-player-mac-wip$SUFFIX.tar.gz
  scp -p distrib/ohrrpgce-player-mac-wip$SUFFIX.tar.gz $UPLOAD_DEST/ohrrpgce/nightly/
  rm distrib/ohrrpgce-player-mac-wip$SUFFIX.tar.gz

  scp -p distrib/ohrrpgce-mac-util$SUFFIX.zip $UPLOAD_DEST/ohrrpgce/nightly/
  rm distrib/ohrrpgce-mac-util$SUFFIX.zip
}


build_package i386 SDL
build_package x86_64 SDL2
