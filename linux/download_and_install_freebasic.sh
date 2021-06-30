#!/bin/sh

FBC_VERSION=1.07.3

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

wget "https://downloads.sourceforge.net/project/fbc/FreeBASIC-${FBC_VERSION}/Binaries-Linux/FreeBASIC-${FBC_VERSION}-linux-x86_64.tar.gz"
tar -xvf "FreeBASIC-${FBC_VERSION}-linux-x86_64.tar.gz"
cd "FreeBASIC-${FBC_VERSION}-linux-x86_64"
echo "Running FreeBasic installer with sudo..."
sudo ./install.sh -i

cd ..
rm -Rf "${DLTEMP}"

