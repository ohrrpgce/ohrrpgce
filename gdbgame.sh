#!/bin/sh
gdb -x=misc/gdbcmds1.txt -x=misc/gdbcmds2.txt --args ./ohrrpgce-game -log . "${@}"
