#!/bin/sh

# *WARNING* Do not schedule this script as a nightly cron job from the same copy of the
# sources that it updates. That would be equivalent to giving any developer with write
# access to the repository full control of the user-account which this script is run as.
# Instead, configure cron to execute a manually updated copy of this script, and pay
# attention to changes to it.

TODAY=`date "+%Y-%m-%d"`

cd ~/src/nightly

if [ ! -d ohrrpgce ] ; then
  echo nightly snapshot not found, checking out from svn...
  svn checkout svn://gilgamesh.HamsterRepublic.com/ohrrpgce ./ohrrpgce
fi

cd ohrrpgce

svn update > ../nightly-temp.txt
UPDATE=`wc -l < ../nightly-temp.txt`
cat ../nightly-temp.txt
rm ../nightly-temp.txt

if [ ${UPDATE} -eq 1 ] ; then
  echo no changes, no need to update snapshot.
  exit
fi

echo removing old nightly source snapshot...
rm ohrrpgce-source-nightly.zip

echo zipping up new nightly snapshot
zip -q -r ohrrpgce-source-nightly.zip ./ohrrpgce
ls -l ohrrpgce-source-nightly.zip

echo uploading new nightly snapshot
scp -p ohrrpgce-source-nightly.zip spam@brionne.cyberverse.com:web/html/ohrrpgce/nightly/

echo uploading plotscripting docs
scp -p ohrrpgce/docs/plotdict.xml spam@brionne.cyberverse.com:web/html/ohrrpgce/docs/
scp -p ohrrpgce/docs/htmlplot.xsl spam@brionne.cyberverse.com:web/html/ohrrpgce/docs/

