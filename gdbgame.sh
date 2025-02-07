#!/bin/sh
DIR=`echo "${0}" | sed -e s/"gdbgame.sh$"/""/`
cd $DIR
gdb -x=misc/gdbcmds1.txt --args ./ohrrpgce-game --rawexx --log . "${@}"
if [ -f "g_debug.txt" ]; then
    echo
    ERRORS="$(grep " ! " g_debug.txt)"
    COUNT=$(echo "$ERRORS" | wc -l)
    echo "g_debug.txt left behind with $COUNT error lines. First:"
    echo "$ERRORS" | head -n 10
fi
