#!/bin/sh

TODAY=`date "+%Y-%m-%d"`
CODE=`cat codename.txt | tr -d "\r"`

if [ ! -f distrib-mac.sh ] ; then
  echo You should only run this script from the ohrrpgce directory.
  exit 1
fi

echo Building binaries
make || exit 1
./makeutil.sh || exit 1
./bundle-apps.sh || exit 1

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
mkdir -p tmp

echo Erasing old distribution files
rm -f distrib/OHRRPGCE*.dmg

echo "Packaging binary distribution of CUSTOM"

echo "  Including binaries"
cp -pR OHRRPGCE-Custom.app tmp
cp -pR OHRRPGCE-Game.app tmp
#cp -p unlump tmp
#cp -p relump tmp

echo "  Including hspeak"
tar -xf mac/utilities.tar.gz -C tmp hspeak

echo "  Including support files"
cp -p plotscr.hsd tmp
cp -p scancode.hsi tmp

echo "  Including readmes"
cp -p README-game.txt tmp
cp -p README-custom.txt tmp
cp -p LICENSE-binary.txt tmp
cp -p whatsnew.txt tmp
if [ $CODE == 'wip' ] ; then
  cp -p IMPORTANT-nightly.txt tmp
fi

echo "  Including Vikings of Midgard"
./relump vikings/vikings.rpgdir tmp/vikings.rpg
cp -pR "vikings/Vikings script files" tmp
cp -p "vikings/README-vikings.txt" tmp

echo "  Including import"
mkdir tmp/import
cp -pR import/* tmp/import

echo "  Including docs"
mkdir tmp/docs
cp -p docs/*.html tmp/docs
cp -p docs/plotdict.xml tmp/docs
cp -p docs/htmlplot.xsl tmp/docs
cp -p docs/more-docs.txt tmp/docs

echo "Creating disk image"

hdiutil create -srcfolder tmp/ -fs HFS+ distrib/OHRRPGCE-$TODAY-$CODE.dmg

#mv tmp ohrrpgce
#tar -jcf distrib/ohrrpgce-mac-x86-$TODAY-$CODE.tar.bz2 ohrrpgce --exclude .svn
#mv ohrrpgce tmp

echo "Erasing contents of temporary directory"
rm -Rf tmp/*
