#!/bin/sh

ARCH="${1}"

if [ "${ARCH}" = "64" -o "${ARCH}" = "32" ] ; then
  echo "Selected ${ARCH}bit version of Euphoria"
else
  echo "You must specify which version of euphoria to install on the command-line. 32 bit or 64 bit. For example:"
  echo "  ./download_and_install_euphoria.sh 32"
  echo "  ./download_and_install_euphoria.sh 64"
  exit 1
fi

if [ -x "`which euc`" ] ; then
  echo "Euphoria seems to already be installed."
  exit 1
fi

if [ ! -x "`which wget`" ] ; then
  echo "wget is not installed. Please install it first"
  exit 1
fi

DLTEMP="euphoria_installer.tmp"
if [ -d "${DLTEMP}" ] ; then
  echo "Looks like this script was already run before... cleaning up."
  rm -Rf "${DLTEMP}"
fi
mkdir "${DLTEMP}"
cd "${DLTEMP}"

if [ "${ARCH}" = 64 ] ; then
  URLARCH="x64"
else
  URLARCH="x86"
fi
URL="https://downloads.sourceforge.net/project/rapideuphoria/Euphoria/4.1.0-beta2/euphoria-4.1.0-Linux-${URLARCH}-57179171dbed.tar.gz"
TARBALL=$(basename "${URL}")
EXTRACTDIR=$(echo ${TARBALL} | sed -e s/"-[a-f0-9]\+.tar.gz"/""/)
wget "${URL}"
tar -xf "${TARBALL}"

if [ -d "${EXTRACTDIR}" ] ; then
  echo "Downloaded and extracted ${EXTRACTDIR}"
else
  echo "Unable to extract ${EXTRACTDIR} from ${TARBALL}"
  exit 1
fi

EUDEST="/usr/local/share/euphoria"
EUBIN="/usr/local/bin"

if [ -x "`which yum`" ] ; then
  echo "###################################################"
  echo "WARNING: Euphoria causes SELinux execheap warnings"
  echo "on Fedora systems. It will still work for compiling"
  echo "hspeak, but SELinux will be grouchy about it."
fi

echo "###################################################"
echo "Installing Euphoria in $EUDEST"
echo "If this is okay, type Y or type anything else to cancel"
read ASKOK
if [ "${ASKOK}" != "Y" -a "${ASKOK}" != "y" ] ; then
  echo "Giving up."
  exit 1
fi
sudo mv "${EXTRACTDIR}" "${EUDEST}"
sudo chown -R root:root "${EUDEST}"
sudo mv "${EUDEST}"/bin/eub "${EUBIN}"
sudo mv "${EUDEST}"/bin/eubind "${EUBIN}"
sudo mv "${EUDEST}"/bin/euc "${EUBIN}"
sudo mv "${EUDEST}"/bin/eucoverage "${EUBIN}"
sudo mv "${EUDEST}"/bin/eudis "${EUBIN}"
sudo mv "${EUDEST}"/bin/eudist "${EUBIN}"
sudo mv "${EUDEST}"/bin/eui "${EUBIN}"
sudo mv "${EUDEST}"/bin/eushroud "${EUBIN}"
sudo mv "${EUDEST}"/bin/eutest "${EUBIN}"
sudo mkdir "/etc/euphoria"
echo "${EUDEST}"/include > "eu.cfg"
sudo mv "eu.cfg" "/etc/euphoria/"
sudo chown root:root "/etc/euphoria/eu.cfg"

cd ..
rm -Rf "${DLTEMP}"

