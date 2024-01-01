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
  docker/ohrrpgce-build-env-android/andr-oldstyle.sh -c '/src/fix_symlinks.sh' || exit 1
  docker/ohrrpgce-build-env-android/andr-oldstyle.sh -sb -c '/src/ohr/distrib-nightly-android.sh' || exit 1
  docker/ohrrpgce-build-env-android/andr-oldstyle.sh -sb -c '/src/ohr/distrib-nightly-android.sh --chromebook' || exit 1
  scp -pr distrib/ohrrpgce-game-android*-debug*.apk james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/

fi | tee "${SCRIPTDIR}"/wrap-nightly-android-output.txt
"${SCRIPTDIR}"/curl_smtp_wrapper.sh "${SCRIPTDIR}"/wrap-nightly-android-output.txt
