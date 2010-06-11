#!/bin/sh
rm *.deb

if [ -d ohrrpgce ] ; then
  rm -R ohrrpgce
fi
python2.6 ./ohrrpgce.py

if [ -d wandering-hamster ] ; then
  rm -R wandering-hamster
fi
python2.6 ./wander.py

if [ -d vikings-of-midgard ] ; then
  rm -R vikings-of-midgard
fi
python2.6 ./viking.py
