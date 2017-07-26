#!/bin/sh

EUDEST="/usr/local/share/euphoria"

echo "Un-installing Euphoria from $EUDEST"
echo ""
echo "This will only work safely if you installed euphoria using"
echo "download_and_install_euphoria.sh and not if you installed"
echo "using some other method. Proceed with caution!"
echo ""
echo "If this is okay, type Y or type anything else to cancel"
read ASKOK
if [ "${ASKOK}" != "Y" -a "${ASKOK}" != "y" ] ; then
  echo "Giving up."
  exit 1
fi

EUBIN="/usr/local/bin"
sudo rm "${EUBIN}"/eub
sudo rm "${EUBIN}"/eubind
sudo rm "${EUBIN}"/euc
sudo rm "${EUBIN}"/eucoverage
sudo rm "${EUBIN}"/eudis
sudo rm "${EUBIN}"/eudist
sudo rm "${EUBIN}"/eui
sudo rm "${EUBIN}"/eushroud
sudo rm "${EUBIN}"/eutest
sudo rm "/etc/euphoria/eu.cfg"
sudo rmdir "/etc/euphoria"
sudo rm -R "/usr/local/share/euphoria"
