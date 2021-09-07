#!/bin/sh
if [ ! -f "plotdict.xml" ] ; then
  WD=`echo "${0}" | sed s/"update-html.sh"//`
  cd "${WD}"
  if [ ! -f "plotdict.xml" ] ; then
    echo "Where is plotdict.xml?!"
    exit 1
  fi
fi

# Update the version
TODAY=`date "+%Y-%m-%d"`
CODE=`cat ../codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
BRANCH=`cat ../codename.txt | grep -v "^#" | head -2 | tail -1 | tr -d "\r"`
sed -i "s/plotscript version=\".*\" datecode=\".*\"/plotscript version=\"$BRANCH\" datecode=\"$TODAY\"/" plotdict.xml

# Validate checks for many things that may not be easily noticable but could
# still cause breakage, like invalid cross links
xmllint --valid --noout plotdict.xml
echo " ----------------- End of xmllint warnings ----------------"
echo
xsltproc plotdict.xml > plotdictionary.html || exit
echo "Successfully produced plotdictionary.html"
