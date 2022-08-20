#!/bin/sh

# This script compiles and uploads linux nightly builds (both x86 and x86_64 by
# default, unless skipped by $OHR_SKIP_X86 and $OHR_SKIP_X86_64) and also all
# platform-independent nightly files, like the plotdict.

# Scheduling this script to run automatically is equivalent to giving the other devs
# write access to your automatic build machine. Don't do it unless you trust them all.
# (which James fortunately does, and the build machine is reasonably sandboxed, so!)

UPLOAD_SERVER="james_paige@motherhamster.org"
UPLOAD_FOLDER="HamsterRepublic.com"
UPLOAD_DEST="$UPLOAD_SERVER:$UPLOAD_FOLDER"

mkdir -p ~/src/nightly
cd ~/src/nightly
echo "Changed to ~/src/nightly"

echo "First package the source distribution..."
if [ ! -d ohrrpgce ] ; then
  echo "Nightly snapshot not found, checking out from svn..."
  mkdir ohrrpgce
  svn checkout https://rpg.hamsterrepublic.com/source/wip ./ohrrpgce/wip
fi

svn cleanup ./ohrrpgce/wip
# Plotdict gets modified by update-html.sh, remove any modifications or conflicts
svn resolve --accept theirs-full --recursive ./ohrrpgce/wip/docs
svn revert --recursive ./ohrrpgce/wip/docs

svn update ./ohrrpgce/wip | tee nightly-temp.txt || exit 1
UPDATE=`grep "Updated to revision" nightly-temp.txt`
rm nightly-temp.txt

if [ -z "$UPDATE" ] ; then
  echo No changes, no need to update nightly.
  exit
fi

cd ohrrpgce/wip

echo "Zipping up new nightly snapshot..."
./ohrpackage.py unix source ../ohrrpgce-source-nightly.zip &&
echo "Uploading nightly source snapshot..." &&
scp -p ../ohrrpgce-source-nightly.zip $UPLOAD_DEST/ohrrpgce/nightly/

# This is duplicated in distrib-nightly-win[-wine].sh except plotdictionary.html
echo uploading plotscripting docs
cd docs
./update-html.sh
cd ../..
scp -p wip/docs/*.png $UPLOAD_DEST/ohrrpgce/nightly/docs/
scp -p wip/docs/plotdict.xml $UPLOAD_DEST/ohrrpgce/nightly/docs/
scp -p wip/docs/htmlplot.xsl $UPLOAD_DEST/ohrrpgce/nightly/docs/
scp -p wip/docs/plotdictionary.html $UPLOAD_DEST/ohrrpgce/nightly/docs/


echo Now we go to build the linux nightlies

cd ..

# Using two checked-out copies of the svn repo ("ohrrpgce-build" for building
# from, "ohrrpgce" kept clean and used for packaging the source) is no longer
# necessary now that we use ohrpackage.py. If they already both exist, use
# ohrrpgce-build, otherwise don't create it.

if [ ! -d ohrrpgce-build ] ; then
  #echo nightly snapshot not found, checking out from svn...
  #mkdir ohrrpgce-build
  #svn checkout https://rpg.hamsterrepublic.com/source/wip ./ohrrpgce-build/wip
  cd ohrrpgce/wip
else
  cd ohrrpgce-build/wip

  svn cleanup
  svn update
fi


# Compile and create .bz2 and .deb files
echo "Calling distrib-linux.sh..."
# Note: this creates a 'full' rather than 'nightly' package. Only Windows nightlies use 'nightly'.
./distrib-linux.sh ohrrpgce-linux-wip ohrrpgce-player-linux || exit 1

if [ -z "$OHR_SKIP_X86" ] ; then
  echo "Uploading 32-bit linux binaries..."
  scp -p distrib/ohrrpgce-linux-wip-x86.tar.bz2 $UPLOAD_DEST/ohrrpgce/nightly/
  scp -p distrib/ohrrpgce-player-linux-x86.zip $UPLOAD_DEST/ohrrpgce/nightly/
fi

if [ -z "$OHR_SKIP_X86_64" ] ; then
  echo "Uploading 64-bit linux binaries..."
  scp -p distrib/ohrrpgce-linux-wip-x86_64.tar.bz2 $UPLOAD_DEST/ohrrpgce/nightly/
  scp -p distrib/ohrrpgce-player-linux-x86_64.zip $UPLOAD_DEST/ohrrpgce/nightly/
fi

echo "Uploading debian packages..."
for arch in i386 amd64 ; do
  # Note: there is no i386 debian package currently.
  if [ -f distrib/ohrrpgce_*.wip-*_$arch.deb ] ; then
    # The deb packages are named ohrrpgce_$YYYY.$MM.$DD.$codename-$revision_$arch.deb,
    # so we need to delete the previous package from the same arch but different filename.
    ssh $UPLOAD_SERVER rm "$UPLOAD_FOLDER/ohrrpgce/nightly/ohrrpgce_*.wip-*_$arch.deb"
    scp -p distrib/ohrrpgce_*.wip-*_$arch.deb $UPLOAD_DEST/ohrrpgce/nightly/
    rm distrib/ohrrpgce_*_$arch.deb
  fi
done

echo "Uploading IMPORTANT-nightly.txt..."
scp -p IMPORTANT-nightly.txt $UPLOAD_DEST/ohrrpgce/nightly/

echo "distrib-nightly-linux.sh is done."
