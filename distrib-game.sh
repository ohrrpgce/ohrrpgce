#!/bin/sh

if [ ! -f distrib.sh ] ; then
  echo You should only run this script from the ohrrpgce directory.
  exit 1
fi

echo Erasing contents of temporary directory
rm -Rf tmp/*

echo Packaging binary distribution of GAME
cp -p game.exe tmp
cp -p ohrrpgce.fnt tmp
cp -p README-game.txt tmp
cp -p LICENSE-binary.txt tmp
cp -p game.ico tmp

echo Erasing old distribution zip file
rm distrib/ohrrpgce_play.zip

echo Zipping up game
cd tmp
zip ../distrib/ohrrpgce_play.zip ./*
cd ..

echo Erasing contents of temporary directory
rm -Rf tmp/*

