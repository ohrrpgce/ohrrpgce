#!/bin/sh

UPLOAD_SERVER="james_paige@motherhamster.org"
UPLOAD_FOLDER="HamsterRepublic.com/ohrrpgce/nightly/"
UPLOAD_DEST="$UPLOAD_SERVER:$UPLOAD_FOLDER"

for VMNAME in "Debian 32bit" "Debian 64bit" "Windows 7" "Mac OS X" ; do
  echo "===Starting ${VMNAME}==="
  vboxmanage startvm "${VMNAME}" --type headless
  RUNNING="True"
  COUNT=1
  while [ -n "${RUNNING}" ] ; do
    sleep 5
    COUNT=$(expr "${COUNT}" + 5)
    RUNNING=$(vboxmanage showvminfo "${VMNAME}" \
      | grep "^State:" \
      | grep "running (since"\
    )
    if [ "${COUNT}" -gt 7200 ] ; then
      echo "ERROR: ${VMNAME} has run for more than 2 hours, killing it!"
      killall "VBoxHeadless"
      sleep 5
      continue
    fi
  done
done

# Also run Android nightly build in docker
~/src/ohr/wip/nightly/wrap-nightly-android.sh

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
