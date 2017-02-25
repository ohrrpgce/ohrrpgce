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
  ./distrib-nightly.sh
fi | tee ~/wrap-nightly-linux-output.txt
/usr/sbin/sendmail < ~/wrap-nightly-linux-output.txt

echo "------------------"
echo "WILL SHUT DOWN NOW"
sleep 5
/sbin/poweroff
