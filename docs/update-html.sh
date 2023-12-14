#!/bin/sh
if [ ! -f "plotdict.xml" ] ; then
  WD=`echo "${0}" | sed s/"update-html.sh"//`
  cd "${WD}"
  if [ ! -f "plotdict.xml" ] ; then
    echo "Where is plotdict.xml?!"
    exit 1
  fi
fi
if [ ! -f "platformdict.xml" ] ; then
  echo "Where is platformdict.xml?!"
  exit 1
fi

# Update the version
TODAY=`date "+%Y-%m-%d"`
CODE=`cat ../codename.txt | grep -v "^#" | head -1 | tr -d "\r"`
BRANCH=`cat ../codename.txt | grep -v "^#" | head -2 | tail -1 | tr -d "\r"`
sed -i "s/plotscript version=\".*\" datecode=\".*\"/plotscript version=\"$CODE\" datecode=\"$TODAY\"/" plotdict.xml
sed -i "s/plotscript version=\".*\" datecode=\".*\"/plotscript version=\"$CODE\" datecode=\"$TODAY\"/" platformdict.xml

# Validate checks for many things that may not be easily noticable but could
# still cause breakage, like invalid cross links
echo " ---------------- plotdict.xml xmllint warnings: ----------------"
xmllint --valid --noout plotdict.xml
echo " -------------- platformdict.xml xmllint warnings: --------------"
xmllint --valid --noout platformdict.xml
echo " ----------------------------------------------------------------"
echo
xsltproc plotdict.xml > plotdictionary.html || exit
echo "Successfully produced plotdictionary.html"
# Do not bother producing a platformdict.html since we don't need it for distribution.
