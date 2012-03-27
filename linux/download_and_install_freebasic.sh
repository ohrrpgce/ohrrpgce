#!/bin/sh

if [ -x "`which fbc`" ] ; then
  echo "FreeBasic seems to already be installed."
  exit 1
fi

if [ ! -x "`which wget`" ] ; then
  echo "wget is not installed. Please install it first"
  exit 1
fi

DLTEMP="freebasic_installer.tmp"
if [ -d "${DLTEMP}" ] ; then
  echo "Looks like this script was already run before... cleaning up."
  rm -Rf "${DLTEMP}"
fi
mkdir "${DLTEMP}"
cd "${DLTEMP}"

wget "http://downloads.sourceforge.net/project/fbc/Binaries%20-%20Linux/FreeBASIC-0.23.0-linux.run?use_mirror=iweb"
chmod +x "FreeBASIC-0.23.0-linux.run"
sudo ./"FreeBASIC-0.23.0-linux.run" install

cd ..
rm -Rf "${DLTEMP}"

