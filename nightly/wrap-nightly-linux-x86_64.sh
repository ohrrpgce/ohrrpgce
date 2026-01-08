#!/bin/sh
echo "ABOUT TO DO A NIGHTLY WIP BUILD"
echo "-------------------------------"
sleep 10

SCRIPTDIR="${0%/*}"
SCRIPTDIR="$(realpath $SCRIPTDIR)"
cd "${SCRIPTDIR}"/..

if [ -n "True" ] ; then
  echo "From: cron@rpg.hamsterrepublic.com"
  echo "To: cron@rpg.hamsterrepublic.com"
  echo "Subject: OHRRPGCE Linux 64-bit nightly build ($(uname -n))"
  echo ""

  svn cleanup
  svn update --trust-server-cert --non-interactive | tee nightly-temp.txt || exit 1
  UPDATE=`grep "Updated to revision" nightly-temp.txt`
  rm nightly-temp.txt
  if [ -z "$UPDATE" ] ; then
    echo No changes, no need to update nightly.
    exit 2
  fi

  echo "Currently in dir:"
  pwd
  OHRDIR="$(pwd)" docker/ohrrpgce-build-env-linux-x86_64/lin64.sh -c '/src/ohr/distrib-linux.sh ohrrpgce-linux-wip ohrrpgce-player-linux-wip' || exit 1
  echo "Currently in dir:"
  pwd
  
  echo "Uploading 64-bit linux binaries..."
  UPLOAD_SERVER=james_paige@motherhamster.org
  UPLOAD_FOLDER=HamsterRepublic.com
  UPLOAD_DEST="$UPLOAD_SERVER:$UPLOAD_FOLDER"
  scp -i ~/.ssh/ohrrpgce_upload -p distrib/ohrrpgce-linux-wip-x86_64.tar.bz2 $UPLOAD_DEST/ohrrpgce/nightly/
  scp -i ~/.ssh/ohrrpgce_upload -p distrib/ohrrpgce-player-linux-wip-x86_64.zip $UPLOAD_DEST/ohrrpgce/nightly/
  echo "Uploading 64-bit deb package..."
  ssh -i ~/.ssh/ohrrpgce_upload "$UPLOAD_SERVER" rm "$UPLOAD_FOLDER/ohrrpgce/nightly/ohrrpgce_*.wip-*_$arch.deb"
  scp -p distrib/ohrrpgce_*.wip-*_$arch.deb $UPLOAD_DEST/ohrrpgce/nightly/
  echo "Uploads complete."

fi 2>&1 | tee ~/wrap-nightly-linux-output.txt
~/src/ohr/wip/nightly/curl_smtp_wrapper.sh ~/wrap-nightly-linux-output.txt
