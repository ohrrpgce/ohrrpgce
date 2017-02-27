#!/bin/sh

for VMNAME in "Debian 64bit" "Windows 7" ; do
  echo "===Starting ${VMNAME}==="
  vboxmanage startvm "${VMNAME}"
  RUNNING="True"
  while [ -n "${RUNNING}" ] ; do
    sleep 5
    RUNNING=$(vboxmanage showvminfo "${VMNAME}" \
      | grep "^State:" \
      | grep "running (since"\
    )
  done
done


