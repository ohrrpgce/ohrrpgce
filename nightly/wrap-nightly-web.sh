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
  echo "Subject: OHRRPGCE Emscripten nightly build ($(uname -n))"
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
  echo "remove old emscripten web nightlies..."
  rm -f distrib/ohrrpgce-player-web-wip.zip
  docker/ohrrpgce-build-env-emscripten/emscr.sh -c '/src/ohr/distrib-nightly-web.sh' || exit 1
  echo "Now to upload ohrrpgce-player-web-wip.zip ..."
  cd "${SCRIPTDIR}"/..
  scp -pr distrib/ohrrpgce-player-web-wip.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/

fi | tee "${SCRIPTDIR}"/wrap-nightly-emscripten-output.txt
"${SCRIPTDIR}"/curl_smtp_wrapper.sh "${SCRIPTDIR}"/wrap-nightly-emscripten-output.txt
