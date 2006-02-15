#!/bin/sh

if [ ! -f distrib.sh ] ; then
  echo You should only run this script from the ohrrpgce directory.
  exit 1
fi

echo Erasing contents of temporary directory
rm -Rf tmp/*

echo Packaging binary distribution of CUSTOM
cp -p game.exe tmp
cp -p game-qb.exe tmp
cp -p custom.exe tmp
cp -p customqb.exe tmp
cp -p hspeak.exe tmp
cp -p unlump.exe tmp
cp -p ohrrpgce.mas tmp
cp -p ohrrpgce.fnt tmp
cp -p ohrrpgce.new tmp
cp -p README-custom.txt tmp
cp -p LICENSE.txt tmp
cp -p LICENSE-binary.txt tmp
cp -p whatsnew.txt tmp
cp -p sample/sample.rpg tmp
cp -p sample/npc_tag.rpg tmp
cp -p sample/pstutor.rpg tmp
cp -p plotscr.hsd tmp
cp -p scancode.hsi tmp

echo Erasing old distribution zip file
rm distrib/custom.zip

echo Zipping up custom
cd tmp
zip ../distrib/custom.zip ./*

cd ..
echo Zipping up import and docs
zip -r distrib/custom.zip import docs -x \*.svn\*

echo Erasing contents of temporary directory
rm -Rf tmp/*

