#!/bin/sh
# Run Game under valgrind's memcheck.
#
# Add --vgdb-error=0 to pause before starting the program to attach gdb.
# Add --vgdb-error=1 to pause and wait for gdb to attach on error (otherwise can't continue)
# Add --show-reachable=yes for more complete memory leak checking.
# With older valgrind, --db-attach=yes instead of --vgdb-error=1 can be convenient.

valgrind --suppressions=misc/valgrind_suppressions.txt --track-fds=yes --read-var-info=yes --gen-suppressions=yes --leak-check=full ./ohrrpgce-game $*
