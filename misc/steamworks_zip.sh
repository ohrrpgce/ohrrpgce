#!/bin/sh

INZIP="$1"
OUTZIP="$2"

if [ ! -f "${INZIP}" ] ; then
  echo "${INZIP} not found"
  echo "Usage:   $0  path/to/steamworks_sdk_###.zip  output.zip"
  echo
  echo "Trims down a copy of the Steamworks SDK to include just the redistributable"
  echo "libraries for Win32, Mac64, Linux32, Linux64. The SDK can be downloaded from"
  echo "https://partner.steamgames.com/doc/sdk after registering."
  exit 1
fi

zip --copy "${INZIP}" --out "${OUTZIP}" '*Readme.txt' '*libsteam_api.*' '*steam_api.dll'
