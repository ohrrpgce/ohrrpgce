#!/bin/bash

UPLOAD_SERVER="james_paige@motherhamster.org"
UPLOAD_FOLDER="HamsterRepublic.com/ohrrpgce/nightly/"
UPLOAD_DEST="$UPLOAD_SERVER:$UPLOAD_FOLDER"

# Sanity-check the docker network, restart daemon if needed
dockerfail() {
  DOCKER_CURL_CHECK=$(docker run --rm curlimages/curl:8.21.0 \
    -sS https://HamsterRepublic.com/ \
    | grep "The Fantastically Adequate Hamster Republic Homepage")
  if [ -z "$DOCKER_CURL_CHECK" ] ; then
    return 0 # Failed, return true
  fi
  return 1 # Ok, return false
}
if dockerfail ; then
  echo "Docker curl test failed, trying to restart docker daemon..."
  # Current user must be set up to do this passwordlessly in /etc/sudoers.d/docker_restart
  # Example:
  # username ALL=(root) NOPASSWD: /usr/bin/systemctl restart docker
  sudo systemctl restart docker
  sleep 10
  if dockerfail ; then
    echo "ERROR: Restarting docker didn't fix networking, there must be a bigger networking problem."
    echo "Quitting since builds are likely to fail."
    exit 1
  fi
fi

# Run nightly source-and-docs (doesn't need to be in docker right now)
~/src/nightly/ohr-source-and-docs/nightly/wrap-nightly-source-and-docs.sh > /dev/null 2>&1

# Run Android nightly build in docker
~/src/nightly/ohr-android/nightly/wrap-nightly-android.sh > /dev/null 2>&1

# Run Web nightly build in docker
~/src/nightly/ohr-web/nightly/wrap-nightly-web.sh > /dev/null 2>&1

# Run Linux 32 bit nightly build in docker
~/src/nightly/ohr-linux-32/nightly/wrap-nightly-linux-x86.sh > /dev/null 2>&1

# Run Linux 64 bit nightly build in docker
~/src/nightly/ohr-linux-64/nightly/wrap-nightly-linux-x86_64.sh > /dev/null 2>&1

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
scp -i ~/.ssh/ohrrpgce_upload -p $SCRIPT_DIR/nightly-check.ini $UPLOAD_DEST

# list the remote directory
ssh -i ~/.ssh/ohrrpgce_upload $UPLOAD_SERVER ls -l $UPLOAD_FOLDER | cut -d " " -f 5-
