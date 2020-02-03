#!/bin/sh

SCRIPTDIR="${0%/*}"
cd "${SCRIPTDIR}"
cd ..

SCPDEST="james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce"

scp -p whatsnew.txt "${SCPDEST}"/

docs/update-html.sh
scp -p docs/plotdict.xml "${SCPDEST}"/docs/
scp -p docs/htmlplot.xsl "${SCPDEST}"/docs/
scp -p docs/plotdictionary.html "${SCPDEST}"/docs/
scp -p docs/*.png "${SCPDEST}"/docs/
