#!/bin/sh
echo "ABOUT TO DO A NIGHTLY WIP BUILD"
echo "-------------------------------"
sleep 10
cd ~/src/ohr/wip

if [ -n "True" ] ; then
  echo "From: cron@rpg.hamsterrepublic.com"
  echo "To: cron@rpg.hamsterrepublic.com"
  echo "Subject: OHRRPGCE Mac nightly build ($(uname -n))"
  echo ""
  # Make sure whave the lates mac distrib script
  # everything else is done in a different folder
  svn update distrib-nightly-mac.sh
  # This hard-codes some paths specific to James's setup
  CC=clang GCC=/opt/local/bin/gcc-mp-4.7 EUDIR=~james/misc/euphoria/ ./distrib-nightly-mac.sh 2>&1
fi | tee ~/wrap-nightly-mac-output.txt

~/src/ohr/wip/nightly/curl_smtp_wrapper.sh ~/wrap-nightly-mac-output.txt

echo "------------------"
echo "WILL SHUT DOWN NOW"
sleep 5
osascript -e 'tell app "System Events" to shut down'
