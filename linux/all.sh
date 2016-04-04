#!/bin/sh
rm *.deb

if [ -d ohrrpgce ] ; then
  rm -R ohrrpgce
fi
python2.7 ./ohrrpgce.py

if [ -d wandering-hamster ] ; then
  rm -R wandering-hamster
fi
python2.7 ./wander.py

if [ -d vikings-of-midgard ] ; then
  rm -R vikings-of-midgard
fi
python2.7 ./viking.py
