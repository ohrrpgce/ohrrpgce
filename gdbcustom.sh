#!/bin/sh
DIR=`echo "${0}" | sed -e s/"gdbcustom.sh$"/""/`
cd "${DIR}"
gdb -x=misc/gdbcmds1.txt --args ./ohrrpgce-custom --rawexx --log . "${@}"
if [ -f "c_debug.txt" ]; then
    echo
    ERRORS="$(grep " ! " c_debug.txt)"
    COUNT=$(echo "$ERRORS" | wc -l)
    echo "c_debug.txt left behind with $COUNT error lines. First:"
    echo "$ERRORS" | head -n 10
fi
