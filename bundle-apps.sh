#!/bin/sh
# This script creates the Mac OSX OHRRPGCE-Custom.app and OHRRPGCE-Game.app apps.
# It should called only after running scons to compile the ohrrpgce-game and
# ohrrpgce-custom binaries.
# Note: only x86 apps are supported currently, because mac/Frameworks.tar.gz
# only contains x86 libraries.

TODAY=`date "+%Y%m%d"`
CODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`

# Sanity checks
if ! file ../ohrrpgce-game | grep i386 ; then
  echo "ohrrpgce-game is missing or not compiled as 32 bit"
  exit 1
fi
if ! file ../ohrrpgce-custom | grep i386 ; then
  echo "ohrrpgce-custom is missing or not compiled as 32 bit"
  exit 1
fi

echo Deleting old apps
rm -rf OHRRPGCE-Game.app
rm -rf OHRRPGCE-Custom.app

echo Bundling OHRRPGCE-Game.app
cp -R mac/OHRRPGCE-Game.app.template OHRRPGCE-Game.app &&
rm -Rf OHRRPGCE-Game.app/.svn &&
rm -Rf OHRRPGCE-Game.app/Contents/.svn &&
rm -Rf OHRRPGCE-Game.app/Contents/Resources/.svn &&
mkdir -p OHRRPGCE-Game.app/Contents/MacOS &&
sed -i -e "s/#VERSION#/O.H.R.RPG.C.E version ${CODE} ${TODAY}/g" OHRRPGCE-Game.app/Contents/Info.plist &&
mkdir -p OHRRPGCE-Game.app/Contents/Resources/ohrhelp &&
cp -R ohrhelp/game_*.txt ohrhelp/share_*.txt ohrhelp/slice*.txt OHRRPGCE-Game.app/Contents/Resources/ohrhelp &&
cp ohrrpgce-game OHRRPGCE-Game.app/Contents/MacOS/ &&
tar xf mac/Frameworks.tar.gz -C OHRRPGCE-Game.app/Contents || exit 1
# Mac tar doesn't have an option to expand symlinks

CWD=`pwd`
echo Un-symlinking OHRRPGCE-Game.app
cd OHRRPGCE-Game.app/Contents/Frameworks/SDL.framework &&
rm -rf Versions/Current SDL Headers Resources &&
cd ../SDL_mixer.framework &&
rm -rf Versions/Current SDL_mixer Headers Frameworks Resources &&
cd Versions/A/Frameworks/mikmod.framework &&
rm -rf Versions/Current mikmod Headers Resources &&
cd $CWD || exit 1

echo Bundling OHRRPGCE-Custom.app
cp -R mac/OHRRPGCE-Custom.app.template OHRRPGCE-Custom.app &&
rm -Rf OHRRPGCE-Custom.app/.svn &&
rm -Rf OHRRPGCE-Custom.app/Contents/.svn &&
rm -Rf OHRRPGCE-Custom.app/Contents/Resources/.svn &&
mkdir -p OHRRPGCE-Custom.app/Contents/MacOS/support &&
mkdir -p OHRRPGCE-Custom.app/Contents/Resources/ohrhelp &&
sed -i -e "s/#VERSION#/O.H.R.RPG.C.E version ${CODE} ${TODAY}/g" OHRRPGCE-Custom.app/Contents/Info.plist &&
cp ohrrpgce-custom OHRRPGCE-Custom.app/Contents/MacOS/ &&
tar xf mac/Frameworks.tar.gz -C OHRRPGCE-Custom.app/Contents &&
cp -R ohrhelp/*.txt OHRRPGCE-Custom.app/Contents/Resources/ohrhelp &&
cp data/* OHRRPGCE-Custom.app/Contents/Resources/ &&
cp support/Terminal_wrapper.sh OHRRPGCE-Custom.app/Contents/MacOS/support/ &&
cp plotscr.hsd scancode.hsi OHRRPGCE-Custom.app/Contents/MacOS/support/ &&
tar xf mac/utilities.tar.gz -C OHRRPGCE-Custom.app/Contents/MacOS/support/ && 
cp hspeak OHRRPGCE-Custom.app/Contents/MacOS/support/ || exit 1
