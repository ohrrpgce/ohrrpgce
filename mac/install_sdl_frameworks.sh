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
  if [ ! -d "/Volumes/$VOLNAME" ] ; then
    echo "Failed to mount $DMG to /Volumes/$VOLNAME"
    return 1
  fi
  
  # I think the first location is the one we really use
  local FWDIR=/Library/Frameworks
  # Delete the old one with reckless abandon
  sudo rm -Rf "$FWDIR/$FWNAME"
  # Copy in the new one
  sudo cp -pr "/Volumes/$VOLNAME/$FWNAME" "$FWDIR/"
  
  # Not sure if this second location gets used, but lets update it too
  local FWDIR=~/Library/Frameworks
  # Delete the old one with reckless abandon
  rm -Rf "$FWDIR/$FWNAME"
  # Copy in the new one
  cp -pr "/Volumes/$VOLNAME/$FWNAME" "$FWDIR/"
  
  # Unmount the DMG
  hdiutil detach "/Volumes/$VOLNAME"
}

installfw "SDL_mixer-1.2.12.dmg" "SDL_mixer"  "SDL_mixer.framework"  "https://www.libsdl.org/projects/SDL_mixer/release"
installfw "SDL2_mixer-2.6.3.dmg" "SDL2_mixer" "SDL2_mixer.framework" "https://www.libsdl.org/projects/SDL_mixer/release"
installfw "SDL-1.2.15.dmg"       "SDL"        "SDL.framework"        "https://www.libsdl.org/release"
installfw "SDL2-2.26.4.dmg"      "SDL2"       "SDL2.framework"       "https://www.libsdl.org/release"

echo ""
echo "All done."
