#!/bin/sh

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

if [ -x "`which dpkg`" ] ; then
  wget "http://downloads.sourceforge.net/project/rapideuphoria/Euphoria/4.0.3/Linux/euphoria_4.0.3_i386.deb"
  sudo dpkg -i euphoria_4.0.3_i386.deb
else
  wget "http://downloads.sourceforge.net/project/rapideuphoria/Euphoria/4.0.3/Linux/euphoria-4.0.3-Linux-ix86.tar.bz2"
  tar -jxf "euphoria-4.0.3-Linux-ix86.tar.bz2"

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
  sudo mv euphoria-4.0.3-Linux-ix86 "${EUDEST}"
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
fi

cd ..
rm -Rf "${DLTEMP}"

