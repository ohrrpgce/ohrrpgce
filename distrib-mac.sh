#!/bin/sh

TODAY=`date "+%Y-%m-%d"`
CODE=`cat codename.txt | tr -d "\r"`

if [ ! -f distrib-mac.sh ] ; then
  echo You should only run this script from the ohrrpgce directory.
  exit 1
fi

echo Building binaries
scons
scons hspeak unlump relump
echo Bundling apps
./bundle-apps.sh

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
mkdir -p distrib

echo Erasing old distribution files
rm -f distrib/OHRRPGCE*.dmg
rm -f distrib/ohrrpgce-mac-minimal*.zip

echo "Packaging binary distribution of CUSTOM"

echo "  Including binaries"
cp -pR OHRRPGCE-Custom.app tmp
cp -pR OHRRPGCE-Game.app tmp

echo "  Including readmes"
cp -p README-mac.txt tmp
cp -p LICENSE-binary.txt tmp
cp -p whatsnew.txt tmp
if [ $CODE == 'wip' ] ; then
  cp -p IMPORTANT-nightly.txt tmp
fi

echo "  Including Vikings of Midgard"
mkdir tmp/"Vikings of Midgard"
./relump vikings/vikings.rpgdir tmp/"Vikings of Midgard"/vikings.rpg
cp -pR "vikings/Vikings script files" tmp/"Vikings of Midgard"
cp -p "vikings/README-vikings.txt" tmp/"Vikings of Midgard"

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

mv tmp OHRRPGCE-$CODE
#tar -jcf distrib/ohrrpgce-mac-x86-$TODAY-$CODE.tar.bz2 ohrrpgce --exclude .svn
hdiutil create -srcfolder OHRRPGCE-$CODE/ -fs HFS+ distrib/OHRRPGCE-$TODAY-$CODE.dmg
mv OHRRPGCE-$CODE tmp

echo "Erasing contents of temporary directory"
rm -Rf tmp/*

echo "Create minimal player tarball"
tar -zcf distrib/ohrrpgce-mac-minimal-$TODAY-$CODE.tar.gz OHRRPGCE-Game.app README-mac-minimal.txt LICENSE-binary.txt

echo "Creating utilities zip"
zip distrib/utilities-mac.zip unlump relump hspeak plotscr.hsd scancode.hsi LICENSE-binary.txt
