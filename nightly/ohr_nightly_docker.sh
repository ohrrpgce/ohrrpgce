#!/bin/sh

UPLOAD_SERVER="james_paige@motherhamster.org"
UPLOAD_FOLDER="HamsterRepublic.com/ohrrpgce/nightly/"
UPLOAD_DEST="$UPLOAD_SERVER:$UPLOAD_FOLDER"

# Run Android nightly build in docker
~/src/nightly/ohr-android/nightly/wrap-nightly-android.sh > /dev/null 2>&1

# Run Web nightly build in docker
~/src/nightly/ohr-web/nightly/wrap-nightly-web.sh > /dev/null 2>&1

# After the nightly build finishes, generate nightly-check.ini listing the svn_rev
# and build_date for the main builds, and upload and email it

SCRIPT_DIR=$(dirname "$0")
$SCRIPT_DIR/check_nightly_wip.sh 2>&1 | tee $SCRIPT_DIR/nightly-check.ini

if [ -n "True" ] ; then
  echo "From: cron@rpg.hamsterrepublic.com"
  echo "To: cron@rpg.hamsterrepublic.com"
  echo "Subject: OHRRPGCE Nightly build check ($(uname -n))"
  echo ""
  cat $SCRIPT_DIR/nightly-check.ini
fi > ~/wrap-nightly-check-output.txt
~/src/ohr/wip/nightly/curl_smtp_wrapper.sh ~/wrap-nightly-check-output.txt
scp -p $SCRIPT_DIR/nightly-check.ini $UPLOAD_DEST

# list the remote directory
ssh $UPLOAD_SERVER ls -l $UPLOAD_FOLDER | cut -d " " -f 5-
