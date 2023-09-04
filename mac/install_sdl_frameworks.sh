#!/bin/bash

cd $(dirname $0)
mkdir -p "mac_sdl_framework_downloads"
cd "mac_sdl_framework_downloads"

installfw () {
  local DMG="$1"
  local VOLNAME="$2"
  local FWNAME="$3"
  local URLPATH="$4"
  echo "=== $DMG ==="
  curl -s -S -k -O "$URLPATH/$DMG"
  if [ ! -e "$DMG" ] ; then
    echo "Failed to download $URLPATH/$DMG"
    return 1
  fi
  hdiutil attach "$DMG"
  if [ ! -d "/Volumes/$VOLNAME"] ; then
    echo "Failed to mount $DMG to /Volumes/$VOLNAME"
    return 1
  fi
  # I think the first location is the one we really use
  for FWDIR in /Library/Frameworks ~/Library/Frameworks ; do
    # Delete the old one with reckless abandon
    rm -Rf "$FWDIR/$FWNAME"
    # Copy in the new one
    cp -pr "/Volumes/$VOLNAME/$FWNAME" "$FWDIR/"
  done
  hdiutil detach "/Volumes/$VOLNAME"
}

installfw "SDL_mixer-1.2.12.dmg" "SDL_mixer"  "SDL_mixer.framework"  "https://www.libsdl.org/projects/SDL_mixer/release"
installfw "SDL2_mixer-2.6.3.dmg" "SDL2_mixer" "SDL2_mixer.framework" "https://www.libsdl.org/projects/SDL_mixer/release"
installfw "SDL-1.2.15.dmg"       "SDL"        "SDL.framework"        "https://www.libsdl.org/release"
installfw "SDL2-2.28.3.dmg"      "SDL2"       "SDL2.framework"       "https://www.libsdl.org/release"

echo ""
echo "All done."
