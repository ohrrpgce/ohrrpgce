#!/bin/sh
#
# Build and package builds for linux

SCONS_ARGS="release=1"

if [ ! -f distrib-linux.sh ] ; then
  echo You should only run this script from the ohrrpgce directory.
  exit 1
fi

echo "Building relump"
scons $SCONS_ARGS lto=1 relump || exit 1

echo "Lumping Vikings of Midgard"
if [ -f vikings.rpg ] ; then
  rm vikings.rpg
fi
./relump vikings/vikings.rpgdir ./vikings.rpg || exit 1

echo "Erasing contents of temporary directory"
mkdir -p tmp
mkdir -p distrib
rm -Rf tmp/*

echo Erasing old distribution files
rm -f distrib/ohrrpgce-linux-*.tar.bz2
rm -f distrib/ohrrpgce-player-*.zip
rm -f distrib/*.deb

package_for_arch() {
  ARCH=$1

  echo "Building $ARCH binaries"
  # lto=1 to reduce unlump/relump size
  scons $SCONS_ARGS arch=$ARCH lto=1 unlump relump || return 1
  scons $SCONS_ARGS arch=$ARCH game custom hspeak || return 1

  echo "Packaging $ARCH binary distribution of CUSTOM"

  echo "  Including binaries"
  cp -p ohrrpgce-game tmp &&
  cp -p ohrrpgce-custom tmp &&
  cp -p unlump tmp &&
  cp -p relump tmp || return 1

  echo "  Including hspeak"
  cp -p hspeak tmp || return 1

  echo "  Including support files"
  cp -p plotscr.hsd tmp &&
  cp -p scancode.hsi tmp || return 1

  echo "  Including readmes"
  cp -p README-game.txt tmp &&
  cp -p README-custom.txt tmp &&
  cp -p IMPORTANT-nightly.txt tmp &&
  cp -p LICENSE.txt tmp &&
  cp -p LICENSE-binary.txt tmp &&
  cp -p whatsnew.txt tmp || return 1

  echo "  Including Vikings of Midgard"
  cp -p vikings.rpg tmp &&
  cp -pr "vikings/Vikings script files" tmp &&
  cp -p "vikings/README-vikings.txt" tmp || return 1

  echo "  Including data files"
  mkdir -p tmp/data &&
  cp -pr data/* tmp/data || return 1

  echo "  Including import"
  mkdir -p tmp/import &&
  cp -pr import/* tmp/import || return 1

  echo "  Including docs"
  mkdir -p tmp/docs &&
  cp -p docs/*.html tmp/docs &&
  cp -p docs/*.png tmp/docs &&
  cp -p docs/plotdict.xml tmp/docs &&
  cp -p docs/htmlplot.xsl tmp/docs &&
  cp -p docs/more-docs.txt tmp/docs || return 1

  echo "  Including help files"
  cp -pr ohrhelp tmp || return 1

  echo "tarring and bzip2ing $ARCH distribution"
  TODAY=`date "+%Y-%m-%d"`
  CODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
  BRANCH=`cat codename.txt | grep -v "^#" | head -2 | tail -1 | tr -d "\r"`
  mv tmp ohrrpgce
  tar -jcf distrib/ohrrpgce-linux-$TODAY-$CODE-$ARCH.tar.bz2 --exclude .svn ./ohrrpgce || return 1
  mv ohrrpgce tmp

  echo "Erasing contents of temporary directory"
  rm -Rf tmp/*

  echo "Prepare minimal $ARCH player zip"
  cp ohrrpgce-game tmp/
  strip tmp/ohrrpgce-game
  zip -j distrib/ohrrpgce-player-linux-bin-minimal-$TODAY-$CODE-$ARCH.zip tmp/ohrrpgce-game LICENSE-binary.txt README-player-only.txt
  rm tmp/ohrrpgce-game
}

if [ -z "${OHR_SKIP_X86}" ] ; then
  package_for_arch x86
fi

if [ -z "${OHR_SKIP_X86_64}" ] ; then
  package_for_arch x86_64 &&
  if which dpkg > /dev/null; then
    echo "Building x86_64 Debian/Ubuntu packages"
    cd linux
    rm -f *.deb
    rm -Rf ohrrpgce
    python2.7 ./ohrrpgce.py || exit 1
    cd ..
    mv linux/*.deb distrib
  fi
fi
