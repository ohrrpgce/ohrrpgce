#!/bin/sh
DIR=`echo "${0}" | sed -e s/"gdbgame.sh$"/""/`
cd $DIR
gdb -x=misc/gdbcmds1.txt -x=misc/gdbcmds2.txt --args ./ohrrpgce-game -log . "${@}"
