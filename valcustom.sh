#!/bin/sh
# Run Custom under valgrind's memcheck.
#
# Add --vgdb-error=0 to pause before starting the program to attach gdb.
# Add --vgdb-error=1 to pause and wait for gdb to attach on error (otherwise can't continue)
# Remove --vgdb-error to pause and ask whether to print a suppression on an error.# Add --show-reachable=yes for more complete memory leak checking.
# With older valgrind, --db-attach=yes instead of --vgdb-error=1 can be convenient.

if [ "$1" = "--no-attach" ]; then
    MOREOPTS=
    shift
else
    echo "Pass --no-attach to avoid suppression prompts or pausing to attach gdb"
    MOREOPTS="--vgdb-error=1 --gen-suppressions=yes"
fi

DIR="$(dirname "$(readlink -e "$0")")"

valgrind --suppressions=$DIR/misc/valgrind_suppressions.txt --track-fds=yes --read-var-info=yes --leak-check=full $MOREOPTS $DIR/ohrrpgce-custom $*

# To run helgrind instead, to find multithreading bugs:
# (NOTE: I recommend compiling with debug=1 music=silence gengcc=1 first.
# gengcc=1 is to get line numbers in the valgrind backtraces.)
#valgrind --tool=helgrind --suppressions=misc/valgrind_suppressions.txt --track-fds=yes --read-var-info=yes --gen-suppressions=yes --vgdb-error=1 ./ohrrpgce-custom $*
