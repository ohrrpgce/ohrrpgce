#!/bin/sh
echo "ABOUT TO DO A NIGHTLY WIP BUILD"
echo "-------------------------------"
sleep 10
cd ~/src/ohr/wip

if [ -n "True" ] ; then
  # This hard-codes some paths specific to James's setup
  CC=clang GCC=/opt/local/bin/gcc-mp-4.7 EUDIR=~james/misc/euphoria/ ./distrib-nightly-mac.sh 2>&1
fi | tee ~/wrap-nightly-mac-output.txt

# You must configure postfix on your mac before the mail command will work.
# The following instructions worked for me:
# http://www.anujgakhar.com/2011/12/09/using-macosx-lion-command-line-mail-with-gmail-as-smtp/
mail \
  -s "Subject: OHRRPGCE Mac nightly build ($(uname -n))" \
  cron@rpg.hamsterrepublic.com \
  < ~/wrap-nightly-mac-output.txt

echo "------------------"
echo "WILL SHUT DOWN NOW"
sleep 5
osascript -e 'tell app "System Events" to shut down'
