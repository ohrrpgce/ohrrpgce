export EUDIR=/usr/share/euphoria
if [ ! -d "${EUDIR}" ] ; then
  echo "Euphoria for Linux was not found installed at $EUDIR"
  echo "If you installed it in a different location, you must edit this script."
  echo "If you do not have Euphoria for Linux, you can get it at http://RapidEuphoria.com/"
  exit 1
fi
export PATH=${PATH}:${EUDIR}/bin
export TERM=ansi
exu hspeak.exw "$@"
