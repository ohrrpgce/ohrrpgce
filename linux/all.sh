#!/bin/sh
rm *.deb
rm -R ohrrpgce
./ohrrpgce.py
rm -R wandering-hamster
./wander.py
rm -R vikings-of-midgard
./viking.py