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
  echo "Subject: OHRRPGCE Linux nightly build (32-bit)"
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
  OHRDIR="$(pwd)" docker/ohrrpgce-build-env-x86/lin32.sh -c '/src/ohr/distrib-linux.sh ohrrpgce-linux-wip ohrrpgce-player-linux-wip' || exit 1
  echo "Currently in dir:"
  pwd
  
  echo "Uploading 32-bit linux binaries..."
  UPLOAD_DEST=james_paige@motherhamster.org:HamsterRepublic.com
  scp -i ~/.ssh/ohrrpgce_upload -p distrib/ohrrpgce-linux-wip-x86.tar.bz2 $UPLOAD_DEST/ohrrpgce/nightly/
  scp -i ~/.ssh/ohrrpgce_upload -p distrib/ohrrpgce-player-linux-wip-x86.zip $UPLOAD_DEST/ohrrpgce/nightly/
  echo "Upload complete."

fi 2>&1 | tee ~/wrap-nightly-linux-output.txt
~/src/ohr/wip/nightly/curl_smtp_wrapper.sh ~/wrap-nightly-linux-output.txt
