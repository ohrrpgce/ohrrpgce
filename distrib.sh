#!/bin/sh

if [ ! -f distrib.sh ] ; then
  echo You should only run this script from the ohrrpgce directory.
  exit 1
fi

echo Erasing contents of temporary directory
rm -Rf tmp/*

echo Packaging binary distribution of CUSTOM
cp -p ohrrpgce.mas tmp
cp -p ohrrpgce.fnt tmp
cp -p ohrrpgce.new tmp
cp -p README-game.txt tmp
cp -p README-custom.txt tmp
cp -p LICENSE.txt tmp
cp -p LICENSE-binary.txt tmp
cp -p whatsnew.txt tmp
cp -p sample/sample.rpg tmp
cp -p sample/npc_tag.rpg tmp
cp -p sample/pstutor.rpg tmp
cp -p plotscr.hsd tmp
cp -p scancode.hsi tmp

echo Erasing old distribution files
rm distrib/custom.zip
rm distrib/ohrrpgce-*.tar.bz2

echo Include Windows Binaries
cp -p game.exe tmp
cp -p game-qb.exe tmp
cp -p custom.exe tmp
cp -p customqb.exe tmp
cp -p hspeak.exe tmp
cp -p unlump.exe tmp

echo Zipping up custom
cd tmp
zip ../distrib/custom.zip ./*

cd ..
echo Zipping up import and docs
zip -r distrib/custom.zip import docs -x \*.svn\*

echo Removing windows binaries
rm tmp/*.exe

echo Including Linux binaries
cp -p ohrrpgce-game tmp
cp -p ohrrpgce-custom tmp
echo Including import
mkdir tmp/import
mkdir tmp/docs
cp -p import/* tmp/import
cp -p docs/* tmp/docs

echo tarring and bzip2ing distribution
mv tmp ohrrpgce
tar -jcf distrib/ohrrpgce-linux-x86.tar.bz2 ./ohrrpgce --exclude .svn
mv ohrrpgce tmp

echo Erasing contents of temporary directory
rm -Rf tmp/*

