#!/bin/sh

if [ ! -f distrib.sh ] ; then
  echo You should only run this script from the ohrrpgce directory.
  exit 1
fi

echo Building binaries
./makegame.sh || exit 1
./makeedit.sh || exit 1
./makeutil.sh || exit 1

echo Erasing contents of temporary directory
rm -Rf tmp/*

echo Packaging binary distribution of CUSTOM
cp -p ohrrpgce.mas tmp
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
rm distrib/ohrrpgce-*.tar.bz2

echo Including Linux binaries
cp -p ohrrpgce-game tmp
cp -p ohrrpgce-custom tmp
cp -p unlump tmp
cp -p relump tmp

echo including hspeak
cp -p hspeak.exw tmp
cp -p hsspiffy.e tmp
cp -p hspeak.sh tmp

echo Including import
mkdir tmp/import
mkdir tmp/docs
cp -p import/* tmp/import
cp -p docs/* tmp/docs

echo tarring and bzip2ing distribution
mv tmp ohrrpgce
tar -jcf distrib/ohrrpgce-linux-x86.tar.bz2 ./ohrrpgce --exclude .svn
mv ohrrpgce tmp

TODAY=`date "+%Y-%m-%d"`
CODE=`cat codename.txt | tr -d "\r"`
mv distrib/ohrrpgce-linux-x86.tar.bz2 distrib/ohrrpgce-linux-x86-$TODAY-$CODE.tar.bz2

echo Erasing contents of temporary directory
rm -Rf tmp/*

