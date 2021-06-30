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

wget "https://downloads.sourceforge.net/project/fbc/Binaries%20-%20Linux/FreeBASIC-1.05.0-linux-x86_64.tar.gz"
wget "https://downloads.sourceforge.net/project/fbc/FreeBASIC-1.08.0/Binaries-Linux/FreeBASIC-1.08.0-linux-x86_64.tar.gz"
tar -xvf "FreeBASIC-1.08.0-linux-x86_64.tar.gz"
cd "FreeBASIC-1.08.0-linux-x86_64"
echo "Running FreeBasic installer with sudo..."
sudo ./install.sh -i

cd ..
rm -Rf "${DLTEMP}"

