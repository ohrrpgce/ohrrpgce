#!/bin/sh
# This script creates the Mac OSX OHRRPGCE-Custom.app and OHRRPGCE-Game.app apps.
# It should called only after running scons to compile the ohrrpgce-game and
# ohrrpgce-custom binaries.

usage() {
  echo "Usage: ./bundle-apps.sh [i386|x86_64] [SDL|SDL2]"
  exit 1
}

TODAY=`date "+%Y%m%d"`
CODE=`cat codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
BRANCH=`cat codename.txt | grep -v "^#" | head -2 | tail -1 | tr -d "\r"`

ARCH=${1:-i386}
[ $ARCH = "i386" -o $ARCH = "x86_64" ] || usage

SDL=${2:-SDL}
[ $SDL = "SDL" -o $SDL = "SDL2" ] || usage

echo Deleting old apps
rm -rf OHRRPGCE-Game.app
rm -rf OHRRPGCE-Custom.app

# Sanity checks
for BINARY in ohrrpgce-game ohrrpgce-custom hspeak; do
  if ! file $BINARY | grep $ARCH ; then
    echo "$BINARY is missing or not compiled for $ARCH"
    exit 1
  fi
done

find_framework() {
  NAME=$1
  SRC="$HOME/Library/Frameworks/$NAME"
  [ -d "$SRC" ] && echo "$SRC" && return 0
  SRC="/Library/Frameworks/$NAME"
  [ -d "$SRC" ] && echo "$SRC" && return 0
  echo "Can't find $NAME"
  exit 1
}

thin_binary() {
  # Remove all archs from a fat binary except $ARCH
  echo thin_binary $1
  # Check it's a fat ("universal") binary, otherwise lipo throws an error
  if file "$1" | grep -q universal ; then
    lipo "$1" -thin $ARCH -output "$1.temp" &&
    mv "$1.temp" "$1" || exit 1
  fi
}

thin_framework() {
  # Run thin_binary on all binaries in a framework.
  FWORK=$1
  FNAME=${FWORK##*/}  # trim path
  BINARY=$1/Versions/A/${FNAME%.framework}  # trim .framework
  thin_binary $BINARY
  SUBDIR=$1/Versions/A/Frameworks
  if [ -d $SUBDIR ]; then
    for FWORK in $SUBDIR/*; do
      thin_framework $FWORK
    done
  fi
}

add_frameworks() {
  # Add ${SDL}.framework and ${SDL}_mixer.framework to an .app
  APP=$1
  if [ $ARCH = "i386" -a $SDL = "SDL" ]; then
    # Use our own SDL* 1.2 i386 frameworks, because there are some small differences:
    # SDL_mixer is compiled for OS 10.4+ instead of 10.5+
    # SDL_mixer uses libmad instead of smpeg, which is smaller and doesn't crash on certain mp3s
    # A couple patches have been applied to SDL_mixer, notably to enable module loop points.
    # However, it is missing other fixes because it's not the latest SDL_mixer version
    tar xf mac/Frameworks.tar.gz -C $APP/Contents || exit 1
  else
    mkdir -p $APP/Contents/Frameworks

    find_framework ${SDL}.framework && # sets $SRC
    cp -R $SRC $APP/Contents/Frameworks/ &&
    thin_framework $APP/Contents/Frameworks/${SDL}.framework || exit 1

    find_framework ${SDL}_mixer.framework && # sets $SRC
    cp -R $SRC $APP/Contents/Frameworks/ &&
    thin_framework $APP/Contents/Frameworks/${SDL}_mixer.framework || exit 1

    # Deleting these frameworks causes "Library not loaded: @rpath/FLAC.framework/Versions/A/FLAC"
    # errors. So we would need to recompile to avoid them (TODO)

    # We don't use FLAC
    #rm -rf $APP/Contents/Frameworks/${SDL}_mixer.framework/Versions/A/Frameworks/FLAC.framework

    # Or Opus (SDL2 only)
    #rm -rf $APP/Contents/Frameworks/${SDL}_mixer.framework/Versions/A/Frameworks/Opus.framework
    #rm -rf $APP/Contents/Frameworks/${SDL}_mixer.framework/Versions/A/Frameworks/OpusFile.framework

    # Delete header files. They're about 330kB zipped (SDL 1.2)
    find $APP -name "*.h" -exec rm "{}" ";" || exit 1
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
cp -R ohrhelp/game_*.txt ohrhelp/share_*.txt ohrhelp/slice*.txt ohrhelp/template_*.txt OHRRPGCE-Game.app/Contents/Resources/ohrhelp &&
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
cp hspeak OHRRPGCE-Custom.app/Contents/MacOS/support/ || exit 1

if [ $ARCH = "i386" ]; then
  tar xf mac/utilities.tar.gz -C OHRRPGCE-Custom.app/Contents/MacOS/support/ || exit 1
else
  tar xf mac/utilities-x86_64.tar.gz -C OHRRPGCE-Custom.app/Contents/MacOS/support/ || exit 1
fi

add_frameworks OHRRPGCE-Custom.app
