#!/bin/sh

for VMNAME in "Debian 32bit" "Debian 64bit" "Windows 7" "Mac OS X" "Debian for Android Builds" ; do
  echo "===Starting ${VMNAME}==="
  vboxmanage startvm "${VMNAME}" --type headless
  RUNNING="True"
  COUNT=1
  while [ -n "${RUNNING}" ] ; do
    sleep 5
    COUNT=$(expr "${COUNT}" + 5)
    RUNNING=$(vboxmanage showvminfo "${VMNAME}" \
      | grep "^State:" \
      | grep "running (since"\
    )
    if [ "${COUNT}" -gt 7200 ] ; then
      echo "ERROR: ${VMNAME} has run for more than 2 hours, killing it!"
      killall "VBoxHeadless"
      sleep 5
      continue
    fi
  done
done

# After the nightly build finishes, do some verification
if [ -n "True" ] ; then
  echo "From: cron@rpg.hamsterrepublic.com"
  echo "To: cron@rpg.hamsterrepublic.com"
  echo "Subject: OHRRPGCE Nightly build check ($(uname -n))"
  echo ""
  SCRIPT_DIR=$(dirname "$0")
  $SCRIPT_DIR/check_nightly_wip.sh 2>&1
fi | tee ~/wrap-nightly-check-output.txt
~/src/ohr/wip/nightly/curl_smtp_wrapper.sh ~/wrap-nightly-check-output.txt

# list the remote directory
ssh james_paige@motherhamster.org ls -l HamsterRepublic.com/ohrrpgce/nightly/ | cut -d " " -f 5-
