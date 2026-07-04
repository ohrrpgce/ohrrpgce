#!/bin/sh
echo "ABOUT TO DO A NIGHTLY WIP SOURCE&DOCS DISTRIB"
echo "---------------------------------------------"
sleep 10

SCRIPTDIR="${0%/*}"
SCRIPTDIR="$(realpath $SCRIPTDIR)"
cd "${SCRIPTDIR}"/..

if [ -n "True" ] ; then
  echo "From: cron@rpg.hamsterrepublic.com"
  echo "To: cron@rpg.hamsterrepublic.com"
  echo "Subject: OHRRPGCE souce-and-docs nightly build ($(uname -n))"
  echo ""

  git fetch origin
  CHANGES=$(git rev-list --count wip..origin/wip)
  echo "$CHANGES new commits..."
  if [ "$CHANGES" -le 0 ] ; then
    echo No changes, no need to update nightly.
    exit 2
  fi
  # Plotdict gets modified by update-html.sh, remove any modifications or conflicts
  git checkout -- ./docs
  echo "If any local changes are present, they will be stashed..."
  git stash
  git checkout wip
  git rebase origin/wip

  echo "Currently in dir: $(pwd)"

  UPLOAD_SERVER="james_paige@motherhamster.org"
  UPLOAD_FOLDER="HamsterRepublic.com"
  UPLOAD_DEST="$UPLOAD_SERVER:$UPLOAD_FOLDER"

  echo "Zipping up new nightly snapshot..."
  rm -f ./distrib/ohrrpgce-source-nightly.zip
  ./ohrpackage.py unix source ./distrib/ohrrpgce-source-nightly.zip &&
    echo "Uploading nightly source snapshot..." &&
    scp -p ./distrib/ohrrpgce-source-nightly.zip $UPLOAD_DEST/ohrrpgce/nightly/
  
  echo "Uploading plotscripting docs..."
  cd docs
  ./update-html.sh
  cd ..
  scp -p docs/*.png $UPLOAD_DEST/ohrrpgce/nightly/docs/
  scp -p docs/plotdict.xml $UPLOAD_DEST/ohrrpgce/nightly/docs/
  scp -p docs/htmlplot.xsl $UPLOAD_DEST/ohrrpgce/nightly/docs/
  scp -p docs/plotdictionary.html $UPLOAD_DEST/ohrrpgce/nightly/docs/

  echo "Uploading IMPORTANT-nightly.txt..."
  scp -p IMPORTANT-nightly.txt $UPLOAD_DEST/ohrrpgce/nightly/
  
  echo "Uploading nightly source-and-docs is done."

fi 2>&1 | tee ~/wrap-nightly-source-and-docs-output.txt
~/src/ohr/wip/nightly/curl_smtp_wrapper.sh ~/wrap-nightly-source-and-docs-output.txt
