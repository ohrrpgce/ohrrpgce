#!/bin/sh
echo "ABOUT TO DO A NIGHTLY WIP BUILD"
echo "-------------------------------"
sleep 10
cd ~/src/ohr/wip

if [ -n "True" ] ; then
  echo "From: cron@rpg.hamsterrepublic.com"
  echo "To: cron@rpg.hamsterrepublic.com"
  echo "Subject: OHRRPGCE Android nightly build ($(uname -n))"
  echo ""
  svn cleanup
  svn update distrib-nightly-android.sh nightly
  pwd
  if ./distrib-nightly-android.sh 2>&1; then
    # distrib-nightly-android.sh returns success if there were svn changes and
    # the build succeeded.  In that case have to force the second invocation
    # because we already updated svn.
    ./distrib-nightly-android.sh --force --chromebook 2>&1
  fi
fi | tee ~/wrap-nightly-android-output.txt
~/src/ohr/wip/nightly/curl_smtp_wrapper.sh ~/wrap-nightly-android-output.txt

echo "------------------"
echo "WILL SHUT DOWN NOW"
sleep 5
/sbin/poweroff
