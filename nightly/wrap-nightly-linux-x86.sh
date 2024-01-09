#!/bin/sh
echo "ABOUT TO DO A NIGHTLY WIP BUILD"
echo "-------------------------------"
sleep 10
cd ~/src/ohr/wip

if [ -n "True" ] ; then
  echo "From: cron@rpg.hamsterrepublic.com"
  echo "To: cron@rpg.hamsterrepublic.com"
  echo "Subject: OHRRPGCE Linux nightly build ($(uname -n))"
  echo ""
  svn cleanup
  svn update distrib-nightly-linux.sh nightly
  export OHR_SKIP_X86_64="Yes"
  ./distrib-nightly-linux.sh
fi 2>&1 | tee ~/wrap-nightly-linux-output.txt
~/src/ohr/wip/nightly/curl_smtp_wrapper.sh ~/wrap-nightly-linux-output.txt

echo "------------------"
echo "WILL SHUT DOWN NOW"
sleep 5
/sbin/poweroff
