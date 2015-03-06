#!/bin/sh
if [ ! -f "plotdict.xml" ] ; then
  WD=`echo "${0}" | sed s/"update-html.sh"//`
  cd "${WD}"
  if [ ! -f "plotdict.xml" ] ; then
    echo "Where is plotdict.xml?!"
    exit 1
  fi
fi
# Validate checks for many things that may not be easily noticable but could
# still cause breakage, like invalid cross links
xmllint --valid --noout plotdict.xml
echo " ----------------- End of xmllint warnings ----------------"
echo
xsltproc plotdict.xml > plotdictionary.html || exit
echo "Successfully produced plotdictionary.html"
