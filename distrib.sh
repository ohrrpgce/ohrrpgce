#!/bin/sh
#
# Build and package builds for linux

if [ ! -f distrib.sh ] ; then
  echo You should only run this script from the ohrrpgce directory.
  exit 1
fi

echo Building binaries
scons debug=0 game custom hspeak unlump relump || exit 1

echo "Lumping Vikings of Midgard"
if [ -f vikings.rpg ] ; then
  rm vikings.rpg
fi
./relump vikings/vikings.rpgdir ./vikings.rpg

echo "Downloading import media"
if [ -f import.zip ] ; then
  rm import.zip
fi
if [ -d "import/Music" ] ; then
  rm -Rf "import/Music"
fi
if [ -d "import/Sound Effects" ] ; then
  rm -Rf "import/Sound Effects"
fi
wget -q http://rpg.hamsterrepublic.com/ohrimport/import.zip
unzip -q -d import/ import.zip
rm import.zip

echo "Erasing contents of temporary directory"
rm -Rf tmp/*

echo Erasing old distribution files
rm distrib/ohrrpgce-*.tar.bz2
rm distrib/*.deb

echo "Packaging binary distribution of CUSTOM"

echo "  Including binaries"
cp -p ohrrpgce-game tmp
cp -p ohrrpgce-custom tmp
cp -p unlump tmp
cp -p relump tmp

echo "  Including hspeak"
cp -p hspeak tmp

echo "  Including support files"
cp -p ohrrpgce.new tmp
cp -p plotscr.hsd tmp
cp -p scancode.hsi tmp

echo "  Including readmes"
cp -p README-game.txt tmp
cp -p README-custom.txt tmp
cp -p IMPORTANT-nightly.txt tmp
cp -p LICENSE.txt tmp
cp -p LICENSE-binary.txt tmp
cp -p whatsnew.txt tmp

echo "  Including Vikings of Midgard"
cp -p vikings.rpg tmp
cp -pr "vikings/Vikings script files" tmp
cp -p "vikings/README-vikings.txt" tmp

echo "  Including import"
mkdir tmp/import
cp -pr import/* tmp/import

echo "  Including docs"
mkdir tmp/docs
cp -p docs/*.html tmp/docs
cp -p docs/plotdict.xml tmp/docs
cp -p docs/htmlplot.xsl tmp/docs
cp -p docs/more-docs.txt tmp/docs

echo "  Including help files"
cp -pr ohrhelp tmp

echo "tarring and bzip2ing distribution"
mv tmp ohrrpgce
tar -jcf distrib/ohrrpgce-linux-x86.tar.bz2 ./ohrrpgce --exclude .svn
mv ohrrpgce tmp

TODAY=`date "+%Y-%m-%d"`
CODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
mv distrib/ohrrpgce-linux-x86.tar.bz2 distrib/ohrrpgce-linux-x86-$TODAY-$CODE.tar.bz2

echo "Erasing contents of temporary directory"
rm -Rf tmp/*

echo "Prepare minimal player zip"
cp ohrrpgce-game tmp/
strip tmp/ohrrpgce-game
zip -j distrib/ohrrpgce-player-linux-bin-minimal-$TODAY-$CODE.zip tmp/ohrrpgce-game LICENSE-binary.txt README-linux-bin-minimal.txt
rm tmp/ohrrpgce-game

echo "Building Debian/Ubuntu packages"
cd linux
if [ -f *.deb ] ; then
  rm *.deb
fi
./all.sh
cd ..
mv linux/*.deb distrib
