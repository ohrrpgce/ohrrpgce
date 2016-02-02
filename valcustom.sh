#!/bin/sh
# Run Custom under valgrind's memcheck.
#
# Add --vgdb-error=0 to pause before starting the program to attach gdb.
# Add --show-reachable=yes for more complete memory leak checking.
# With older valgrind, --db-attach=yes instead of --vgdb-error=1 can be convenient.

valgrind --suppressions=misc/valgrind_suppressions.txt --vgdb-error=1 --track-fds=yes --read-var-info=yes --gen-suppressions=yes --leak-check=full ./ohrrpgce-custom $*
