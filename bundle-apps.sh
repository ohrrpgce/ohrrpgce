#!/bin/sh

TODAY=`date "+%Y%m%d"`
CODE=`cat codename.txt | tr -d "\r"`

echo Deleting old apps
rm -rf OHRRPGCE-Game.app
rm -rf OHRRPGCE-Custom.app

echo Bundling OHRRPGCE-Game.app
cp -R mac/OHRRPGCE-Game.app.template OHRRPGCE-Game.app &&
mkdir -p OHRRPGCE-Game.app/Contents/MacOS &&
sed -i -e "s/#VERSION#/O.H.R.RPG.C.E version ${CODE} ${TODAY}/g" OHRRPGCE-Game.app/Contents/Info.plist &&
cp ohrrpgce-game OHRRPGCE-Game.app/Contents/MacOS/ &&
tar xf mac/Frameworks.tar.gz -C OHRRPGCE-Game.app/Contents || exit 1

echo Bundling OHRRPGCE-Custom.app
cp -R mac/OHRRPGCE-Custom.app.template OHRRPGCE-Custom.app &&
mkdir -p OHRRPGCE-Custom.app/Contents/MacOS &&
mkdir -p OHRRPGCE-Custom.app/Contents/Resources/ohrhelp &&
mkdir -p OHRRPGCE-Custom.app/Contents/Resources/support &&
sed -i -e "s/#VERSION#/O.H.R.RPG.C.E version ${CODE} ${TODAY}/g" OHRRPGCE-Custom.app/Contents/Info.plist &&
cp ohrrpgce-custom OHRRPGCE-Custom.app/Contents/MacOS/ &&
tar xf mac/Frameworks.tar.gz -C OHRRPGCE-Custom.app/Contents &&
cp -R ohrhelp/*.txt OHRRPGCE-Custom.app/Contents/Resources/ohrhelp &&
cp ohrrpgce.new OHRRPGCE-Custom.app/Contents/Resources/ &&
tar xf mac/utilities.tar.gz --exclude hspeak -C OHRRPGCE-Custom.app/Contents/Resources/support/ || exit 1
