#!/bin/sh
DIR=`echo "${0}" | sed -e s/"gdbcustom.sh$"/""/`
cd "${DIR}"
gdb -x=misc/gdbcmds1.txt --args ./ohrrpgce-custom --rawexx --log . "${@}"
