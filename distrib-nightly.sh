#!/bin/sh

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

echo removing old nightly binaries...
rm ../ohrrpgce-binary-nightly.zip

echo building dos binaries...
dosemu -dumb -input "cd ohrrpgce\renv-set\rcompile\rexitemu\r" > /dev/null
rm ?ver.txt compat.b*
BINARIES=`svn status | tr -s " "| cut -d " " -f 2-`

echo zipping up binaries...
zip -q ../ohrrpgce-binary-nightly.zip ${BINARIES} LICENSE-binary.txt whatsnew.txt
rm ${BINARIES}

cd ..
ls -l ohrrpgce-binary-nightly.zip

echo removing old nightly source snapshot...
rm ohrrpgce-source-nightly.zip

echo zipping up new nightly snapshot
zip -q -r ohrrpgce-source-nightly.zip ./ohrrpgce
ls -l ohrrpgce-source-nightly.zip

echo uploading new nightly snapshot
scp -p ohrrpgce-source-nightly.zip spam@brionne.cyberverse.com:web/html/ohrrpgce/archive/
scp -p ohrrpgce-binary-nightly.zip spam@brionne.cyberverse.com:web/html/ohrrpgce/archive/

echo uploading plotscripting docs
scp -p ohrrpgce/docs/plotdict.xml spam@brionne.cyberverse.com:web/html/ohrrpgce/docs/
scp -p ohrrpgce/docs/htmlplot.xsl spam@brionne.cyberverse.com:web/html/ohrrpgce/docs/

