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

  git fetch origin
  CHANGES=$(git rev-list --count wip..origin/wip)
  echo "$CHANGES new commits..."
  if [ "$CHANGES" -le 0 ] ; then
    echo No changes, no need to update nightly.
    exit 2
  fi
  echo "If any local changes are present, they will be stashed..."
  git stash
  git checkout wip
  git rebase origin/wip

  pwd
  echo "remove old android nightlies..."
  rm -f distrib/ohrrpgce-game-android*-debug*.apk
  export OHRDIR="$(pwd)"
  docker/ohrrpgce-build-env-android/andr-oldstyle.sh -c '/src/fix_symlinks.sh' || exit 1
  docker/ohrrpgce-build-env-android/andr-oldstyle.sh -sb -c '/src/ohr/distrib-nightly-android.sh' || exit 1
  docker/ohrrpgce-build-env-android/andr-oldstyle.sh -sb -c '/src/ohr/distrib-nightly-android.sh --chromebook' || exit 1
  scp -i ~/.ssh/ohrrpgce_upload -pr distrib/ohrrpgce-game-android*-debug*.apk james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/

fi 2>&1 | tee "${SCRIPTDIR}"/wrap-nightly-android-output.txt
"${SCRIPTDIR}"/curl_smtp_wrapper.sh "${SCRIPTDIR}"/wrap-nightly-android-output.txt
