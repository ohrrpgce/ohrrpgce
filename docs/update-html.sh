#!/bin/sh
if [ ! -f "plotdict.xml" ] ; then
  WD=`echo "${0}" | sed s/"update-html.sh"//`
  cd "${WD}"
  if [ ! -f "plotdict.xml" ] ; then
    echo "Where is plotdict.xml?!"
    exit 1
  fi
fi
xsltproc plotdict.xml > plotdictionary.html
