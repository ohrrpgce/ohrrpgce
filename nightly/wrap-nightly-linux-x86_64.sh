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
  export OHR_SKIP_X86="Yes"
  ./distrib-nightly.sh 2>&1
fi | tee ~/wrap-nightly-linux-output.txt
/usr/sbin/sendmail < ~/wrap-nightly-linux-output.txt

echo "------------------"
echo "WILL SHUT DOWN NOW"
sleep 5
/sbin/poweroff
