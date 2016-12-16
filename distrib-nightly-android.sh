#!/bin/sh

# Paths in this are specific to James's computer, but it works, and is something to start with

cd ~/Dropbox/src/ohr/wip
scons fbc=$FBCARM debug=0 android-source=1 game
scons
cd ~/Dropbox/src/sdl-android/project/jni/application
git checkout ohrrpgce
rm src
ln -s ohrrpgce src
cd ~/Dropbox/src/sdl-android
rm project/bin/MainActivity-debug.apk
./build.sh

if [ "$1" = "-noupload" ] ; then
  echo "skipping upload."
  exit
fi

if [ -f project/bin/MainActivity-debug.apk ] ; then
  scp -pr project/bin/MainActivity-debug.apk james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/ohrrpgce-game-android-debug.apk
fi
