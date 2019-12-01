#!/bin/sh
# This script creates the Mac OSX OHRRPGCE-Custom.app and OHRRPGCE-Game.app apps.
# It should called only after running scons to compile the ohrrpgce-game and
# ohrrpgce-custom binaries.
# Note: only x86 apps are supported currently, because mac/Frameworks.tar.gz
# only contains x86 libraries.

TODAY=`date "+%Y%m%d"`
CODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`

ARCH=${1:-i386}
if [ $ARCH != "i386" -a $ARCH != "x86_64" ]; then
  echo "Usage: ./bundle-apps.sh [i386|x86_64]"
  exit 1
fi

echo Deleting old apps
rm -rf OHRRPGCE-Game.app
rm -rf OHRRPGCE-Custom.app

# Sanity checks
for BINARY in ohrrpgce-game ohrrpgce-custom hpeak; do
  if ! file $BINARY | grep $ARCH ; then
    echo "$BINARY is missing or not compiled for $ARCH"
    exit 1
  fi
done

find_framework() {
  NAME=$1
  temp="~/Library/Frameworks/$NAME"
  [ -d "$temp" ] && return "$temp"
  temp="/System/Library/Frameworks/$NAME"
  [ -d "$temp" ] && return "$temp"
  echo "Can't find $NAME"
  exit 1
}

add_frameworks() {
  APP=$1
  if [ $ARCH = "i386" ]; then
    # Use our own SDL* 1.2 frameworks, because there are some small differences:
    # SDL_mixer is compiled for OS 10.4+ instead of 10.5+
    # SDL_mixer uses libmad instead of smpeg, which is smaller and doesn't crash on certain mp3s
    # A couple patches have been applied to SDL_mixer, notably to enable module loop points.
    # However, it is missing other fixes because it's not the latest SDL_mixer version
    tar xf mac/Frameworks.tar.gz -C $APP/Contents || exit 1
  else
    SRC=find_framework SDL.framework
    mkdir $APP/Contents/Frameworks
    cp -ra $SRC $APP/Contents/Frameworks/

    SRC=find_framework SDL_mixer.framework
    mkdir $APP/Contents/Frameworks
    cp -ra $SRC $APP/Contents/Frameworks/
    # We don't use FLAC
    rm -rf $APP/Contents/Frameworks/SDL_mixer.framework/Versions/A/Frameworks/FLAC.framework

    # TODO: remove unwanted archs from fat binaries
  fi
}

echo Bundling OHRRPGCE-Game.app
cp -R mac/OHRRPGCE-Game.app.template OHRRPGCE-Game.app &&
rm -Rf OHRRPGCE-Game.app/.svn &&
rm -Rf OHRRPGCE-Game.app/Contents/.svn &&
rm -Rf OHRRPGCE-Game.app/Contents/Resources/.svn &&
mkdir -p OHRRPGCE-Game.app/Contents/MacOS &&
sed -i -e "s/#VERSION#/O.H.R.RPG.C.E version ${CODE} ${TODAY}/g" OHRRPGCE-Game.app/Contents/Info.plist &&
mkdir -p OHRRPGCE-Game.app/Contents/Resources/ohrhelp &&
cp -R ohrhelp/game_*.txt ohrhelp/share_*.txt ohrhelp/slice*.txt OHRRPGCE-Game.app/Contents/Resources/ohrhelp &&
cp ohrrpgce-game OHRRPGCE-Game.app/Contents/MacOS/ || exit 1

add_frameworks OHRRPGCE-Game.app

# Remove symlinks, because they are a problem on Windows
# (Mac tar doesn't have an option to expand symlinks)
echo Un-symlinking OHRRPGCE-Game.app
find OHRRPGCE-Game.app -type l -exec rm "{}" ";" || exit 1

echo Bundling OHRRPGCE-Custom.app
cp -R mac/OHRRPGCE-Custom.app.template OHRRPGCE-Custom.app &&
rm -Rf OHRRPGCE-Custom.app/.svn &&
rm -Rf OHRRPGCE-Custom.app/Contents/.svn &&
rm -Rf OHRRPGCE-Custom.app/Contents/Resources/.svn &&
mkdir -p OHRRPGCE-Custom.app/Contents/MacOS/support &&
mkdir -p OHRRPGCE-Custom.app/Contents/Resources/ohrhelp &&
sed -i -e "s/#VERSION#/O.H.R.RPG.C.E version ${CODE} ${TODAY}/g" OHRRPGCE-Custom.app/Contents/Info.plist &&
cp ohrrpgce-custom OHRRPGCE-Custom.app/Contents/MacOS/ &&
cp -R ohrhelp/* OHRRPGCE-Custom.app/Contents/Resources/ohrhelp &&
cp -R data/* OHRRPGCE-Custom.app/Contents/Resources/ &&
cp support/Terminal_wrapper.sh OHRRPGCE-Custom.app/Contents/MacOS/support/ &&
cp plotscr.hsd scancode.hsi OHRRPGCE-Custom.app/Contents/MacOS/support/ &&
tar xf mac/utilities.tar.gz -C OHRRPGCE-Custom.app/Contents/MacOS/support/ && 
cp hspeak OHRRPGCE-Custom.app/Contents/MacOS/support/ || exit 1

add_frameworks OHRRPGCE-Custom.app
