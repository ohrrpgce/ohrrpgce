#!/bin/sh
DIR=`echo "${0}" | sed -e s/"gdbgame.sh$"/""/`
cd $DIR
gdb -x=misc/gdbcmds1.txt --args ./ohrrpgce-game --rawexx --log . "${@}"
