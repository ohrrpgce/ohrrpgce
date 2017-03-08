#!/bin/sh
rm *.deb

if [ -d ohrrpgce ] ; then
  rm -R ohrrpgce
fi
python2.7 ./ohrrpgce.py
