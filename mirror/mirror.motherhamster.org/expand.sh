#!/bin/sh
DIR=$1

cd ~/mirror.motherhamster.org

if [ ! -d "${DIR}" ] ; then
  echo "ERROR: ${DIR} dies not exist, aborting"
  exit 1
fi

if [ ! -f mirror.tar.bz2 ] ; then
  echo "ERROR: mirror.tar.bz2 does not exist, aborting"
  exit 1
fi

mkdir "${DIR}.new"
cd "${DIR}.new"
echo "Decompressing..."
tar -jmxf ../mirror.tar.bz2 && GOOD="true" > /dev/null
cd ..

rm mirror.tar.bz2

if [ "${GOOD}" ] ; then
  mv "${DIR}" "${DIR}.old"
  mv "${DIR}.new" "${DIR}" 
  rm -R "${DIR}.old"
  echo "Done."
else
  rm -R "${DIR}.new"
  echo "ERROR: decompression failed"
  exit 1
fi

