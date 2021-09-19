#!/bin/sh

FBC_VERSION=1.08.1

ARCH="${1}"

if [ "${ARCH}" = "64" -o "${ARCH}" = "32" ] ; then
  echo "Selected ${ARCH}bit version of FreeBasic"
else
  echo "You must specify which version of Freebasic to install on the command-line. 32 bit or 64 bit. For example:"
  echo "  ./download_and_install_freebasic.sh 32"
  echo "  ./download_and_install_freebasic.sh 64"
  exit 1
fi

if [ "${ARCH}" = "64" ] ; then
  ARCH_SUFFIX="x86_64"
else
  ARCH_SUFFIX="x86"
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

wget "https://downloads.sourceforge.net/project/fbc/FreeBASIC-${FBC_VERSION}/Binaries-Linux/FreeBASIC-${FBC_VERSION}-linux-${ARCH_SUFFIX}.tar.gz"
tar -xvf "FreeBASIC-${FBC_VERSION}-linux-${ARCH_SUFFIX}.tar.gz"
cd "FreeBASIC-${FBC_VERSION}-linux-${ARCH_SUFFIX}"
echo "Running FreeBasic installer with sudo..."
sudo ./install.sh -i

cd ..
rm -Rf "${DLTEMP}"

