#!/bin/sh
# Sets LD_LIBRARY_PATH and runs ohrrpgce-custom

PROG=ohrrpgce-custom
OS=linux
DIR="$(dirname "$(readlink -e "$0")")"
COMMAND="$(basename -s .sh "$0")"

# Search for $PROG in the same directory or $OS/$ARCH/
# (This script may be renamed $PROG)
if [ -f "$DIR/$PROG" ] && [ "$COMMAND" != "$PROG" ]; then
    EXE="$DIR/$PROG"
else
    MACHINE="$(uname -m)"
    ARCH=x86
    if [ "$MACHINE" = "x86_64" ] || [ "$MACHINE" = "amd64" ]; then
        # Prefer native if available, but fallback to running an x86 build
        if [ -f "$DIR/$OS/x86_64/$PROG" ]; then
            ARCH=x86_64
        fi
    fi
    EXE="$DIR/$OS/$ARCH/$PROG"
fi
if [ ! -x "$EXE" ]; then
    echo "$PROG is missing or not executable"
    exit 1
fi

export LD_LIBRARY_PATH="$DIR/$OS/x86:$DIR/$OS/x86_64:$LD_LIBRARY_PATH"

exec "$EXE" -appdir "$DIR" "$@"
