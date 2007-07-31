#!/bin/sh
rm *.deb

if [ -d ohrrpgce ] ; then
  rm -R ohrrpgce
fi
./ohrrpgce.py

if [ -d wandering-hamster ] ; then
  rm -R wandering-hamster
fi
./wander.py

if [ -d vikings-of-midgard ] ; then
  rm -R vikings-of-midgard
fi
./viking.py