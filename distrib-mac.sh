#!/bin/sh

# Call with $ARCH set to i386 or x86_64 to change arch,
# and $SDL set to SDL or SDL2 to select gfx backend

ARCH=${ARCH:-i386}
SDL=${SDL:-SDL}

if [ $ARCH = "x86_64" ]; then
  SUFFIX=-x86_64
else
  SUFFIX=-x86
  # Unfortunately a single euphoria installation can only target one arch, so
  # need a second one outside of $PATH. Mac nightly build machine has 64-bit
  # euphoria in PATH
  if [ -d ~/misc/eu32 ]; then
    export EUDIR=~/misc/eu32
    export EUC=$EUDIR/bin/euc
  fi
fi

if [ $SDL = "SDL2" ]; then
  GFX=sdl2
else
  GFX=sdl
fi

EXTRA_SCONS_OPTIONS=$*

TODAY=`date "+%Y-%m-%d"`
CODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
BRANCH=`cat codename.txt | grep -v "^#" | head -2 | tail -1 | tr -d "\r"`

if [ ! -f distrib-mac.sh ] ; then
  echo You should only run this script from the ohrrpgce directory.
  exit 1
fi

echo
echo
echo "Building binaries for ARCH=$ARCH GFX=$GFX"

rm -f ohrrpgce-game ohrrpgce-custom

# Note: lto=1 is used on Windows/Linux to reduce unlump/relump size, but currently
# on Mac it causes an assembler error
scons release=1 ${EXTRA_SCONS_OPTIONS} arch=$ARCH unlump relump hspeak || exit 1
scons release=1 ${EXTRA_SCONS_OPTIONS} arch=$ARCH gfx=$GFX game custom || exit 1

echo "Bundling apps"
./bundle-apps.sh $ARCH $SDL || exit 1

echo "Erasing contents of temporary directory"
rm -Rf tmp/*
mkdir -p tmp
mkdir -p distrib

echo "Erasing old $SUFFIX distribution files"
rm -f distrib/OHRRPGCE-*$SUFFIX.dmg
rm -f distrib/ohrrpgce-mac-minimal-*$SUFFIX.tar.gz
rm -f distrib/ohrrpgce-mac-util$SUFFIX.zip

echo "Packaging binary distribution of CUSTOM"

echo "  Including binaries"
cp -pR OHRRPGCE-Custom.app tmp &&
cp -pR OHRRPGCE-Game.app tmp || exit 1

echo "  Including readmes"
cp -p README-mac.txt tmp
cp -p LICENSE-binary.txt tmp
cp -p whatsnew.txt tmp
if [ $CODE == 'wip' ] ; then
  cp -p IMPORTANT-nightly.txt tmp
fi

echo "  Including import"
mkdir tmp/import
cp -pR import/* tmp/import

echo "  Including docs"
mkdir tmp/docs &&
cp -p docs/*.html tmp/docs &&
cp -p docs/*.png tmp/docs &&
cp -p docs/plotdict.xml tmp/docs &&
cp -p docs/htmlplot.xsl tmp/docs &&
cp -p docs/more-docs.txt tmp/docs || exit 1

echo "Creating disk image"
mv tmp OHRRPGCE-$BRANCH$SUFFIX
hdiutil create -srcfolder OHRRPGCE-$BRANCH$SUFFIX/ -fs HFS+ distrib/OHRRPGCE-$TODAY-$BRANCH$SUFFIX.dmg || exit 1
mv OHRRPGCE-$BRANCH$SUFFIX tmp

echo "Erasing contents of temporary directory"
rm -Rf tmp/*

echo "Generating buildinfo.ini"
rm -f buildinfo.ini
./OHRRPGCE-Game.app/Contents/MacOS/ohrrpgce-game -dump-embed buildinfo.ini

echo "Create minimal player tarball"
gnutar -zcf distrib/ohrrpgce-player-mac-$TODAY-$BRANCH$SUFFIX.tar.gz OHRRPGCE-Game.app buildinfo.ini README-player-only.txt LICENSE-binary.txt || exit 1

echo "Creating utilities archive"
zip distrib/ohrrpgce-mac-util$SUFFIX.zip unlump relump hspeak plotscr.hsd scancode.hsi LICENSE-binary.txt || exit 1

echo "distrib-mac.sh done."
echo
echo "ls distrib"
ls -l distrib
