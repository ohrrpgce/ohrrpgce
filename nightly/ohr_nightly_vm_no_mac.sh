#!/bin/sh

# This copy of the script skips the Mac VM because it is broken in the current version of VirtualBox

for VMNAME in "Debian 32bit" "Debian 64bit" "Windows 7" "Debian for Android Builds" ; do
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

# After the nightly build finishes, list the remote directory
# for a convenient overview of what builds succeeded
ssh james_paige@motherhamster.org ls -l HamsterRepublic.com/ohrrpgce/nightly/ | cut -d " " -f 5-
