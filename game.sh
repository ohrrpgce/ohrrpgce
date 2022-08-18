#!/bin/sh
# Sets LD_LIBRARY_PATH and runs ohrrpgce-game, automatically starting a game if renamed

PROG=ohrrpgce-game
OS=linux
DIR="$(dirname "$(readlink -e "$0")")"
COMMAND="$(basename -s .sh "$0")"

# If this script has been renamed (or is a symlink) load the .rpg/.rpgdir with
# that name, unless an .rpg/.rpgdir is passed as argument.
RPG=
if echo "$@" | grep -qv "\.rpg"; then
    if [ "$COMMAND" != "game" ]; then
        if [ -f "$DIR/$COMMAND.rpg" ]; then
            RPG="$DIR/$COMMAND.rpg"
        elif [ -d "$DIR/$COMMAND.rpgdir" ]; then
            RPG="$DIR/$COMMAND.rpgdir"
        fi
    fi
fi

# Search for $PROG in the same directory or $OS/$ARCH/
if [ -f "$DIR/$PROG" ]; then
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

# Add $RPG as an arg only if not empty
exec "$EXE" ${RPG:+"$RPG"} "$@"
