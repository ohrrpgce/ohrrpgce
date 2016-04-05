#!/bin/sh

# Paths in this are specific to James's computer, but it works, and is something to start with

if [ ! -f "codename.txt" ] ; then
  echo "Run this from the ohrrpgce source dir"
  exit 1
fi

CODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
TODAY=`date "+%Y-%m-%d"`

scons fbc=$FBCARM debug=0 android-source=1 game
scons
if [ ! -d "sdl-android" ] ; then
  git clone https://github.com/bob-the-hamster/commandergenius.git sdl-android
fi
cd sdl-android
git checkout ohrrpgce
cd project/jni/freebasic
rm rtlib
ln -s ~/src/misc/fbc-tmc/src/rtlib rtlib
cd ../application
rm ohrrpgce
ln -s ../../../../android ohrrpgce
rm src
ln -s ohrrpgce src
cd ../../..
rm project/bin/MainActivity-debug.apk
./build.sh
if [ -f project/bin/MainActivity-debug.apk ] ; then
  cp -p project/bin/MainActivity-debug.apk ohrrpgce-game-android-debug-${TODAY}-${CODE}.apk
  scp -pr ohrrpgce-game-android-debug-${TODAY}-${CODE}.apk james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/archive/
fi
