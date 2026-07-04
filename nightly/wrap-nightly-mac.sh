#!/bin/sh
echo "ABOUT TO DO A NIGHTLY WIP BUILD"
echo "-------------------------------"
sleep 10
cd ~/src/nightly/ohrrpgce

if [ -n "True" ] ; then
  echo "From: cron@rpg.hamsterrepublic.com"
  echo "To: cron@rpg.hamsterrepublic.com"
  echo "Subject: OHRRPGCE Mac nightly build ($(uname -n))"
  echo ""
  # Make sure we have the latest mac distrib script
  # everything else is done in a different folder

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

  # This hard-codes some paths specific to James's setup
  CC=clang GCC=/opt/local/bin/gcc-mp-4.7 EUDIR=~james/misc/euphoria/ ./distrib-nightly-mac.sh
fi 2>&1 | tee ~/wrap-nightly-mac-output.txt

~/src/nightly/ohrrpgce/nightly/curl_smtp_wrapper.sh ~/wrap-nightly-mac-output.txt

echo "------------------"
echo "WILL SHUT DOWN NOW"
sleep 5
osascript -e 'tell app "System Events" to shut down'
