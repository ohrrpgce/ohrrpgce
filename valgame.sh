#!/bin/sh
# Run Game under valgrind's memcheck.
#
# Add --vgdb-error=0 to pause before starting the program to attach gdb.
# Add --vgdb-error=1 to pause and wait for gdb to attach on error (otherwise can't continue)
# Remove --vgdb-error to pause and ask whether to print a suppression on an error.
# Add --show-reachable=yes for more complete memory leak checking.
# With older valgrind, --db-attach=yes instead of --vgdb-error=1 can be convenient.

if [ "$1" = "--no-pause" ]; then
    # For spawning from Custom: don't pass any options that would cause valgrind to pause
    MOREOPTS=
    shift
else
    echo "Pass --no-pause to avoid prompts or pausing to attach gdb"
    MOREOPTS="--vgdb-error=1 --gen-suppressions=yes"
fi

DIR="$(dirname "$(readlink -e "$0")")"

valgrind --suppressions=$DIR/misc/valgrind_suppressions.txt --track-fds=yes --read-var-info=yes --leak-check=full $MOREOPTS $DIR/ohrrpgce-game $*
