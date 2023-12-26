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
  svn update --trust-server-cert --non-interactive | tee nightly-temp.txt || exit 1
  UPDATE=`grep "Updated to revision" nightly-temp.txt`
  rm nightly-temp.txt
  if [ -z "$UPDATE" ] ; then
    echo No changes, no need to update nightly.
    exit 2
  fi

  pwd
  echo "remove old android nightlies..."
  rm -f distrib/ohrrpgce-game-android*-debug*.apk
  docker/ohrrpgce-build-env-android/andr-oldstyle.sh -c '/src/ohr/distrib-nightly-android.sh' || exit 1
  docker/ohrrpgce-build-env-android/andr-oldstyle.sh -c '/src/ohr/distrib-nightly-android.sh --chromebook' || exit 1
  scp -pr distrib/ohrrpgce-game-android*-debug*.apk james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/

fi | tee ~/wrap-nightly-android-output.txt
~/src/ohr/wip/nightly/curl_smtp_wrapper.sh ~/wrap-nightly-android-output.txt

echo "------------------"
echo "WILL SHUT DOWN NOW"
sleep 5
/sbin/poweroff
