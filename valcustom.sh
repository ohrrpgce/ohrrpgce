#!/bin/sh
# Run Custom under valgrind's memcheck.
#
# Add --vgdb-error=0 to pause before starting the program to attach gdb.
# Add --vgdb-error=1 to pause and wait for gdb to attach on error (otherwise can't continue)
# Remove --vgdb-error to pause and ask whether to print a suppression on an error.# Add --show-reachable=yes for more complete memory leak checking.
# With older valgrind, --db-attach=yes instead of --vgdb-error=1 can be convenient.

valgrind --suppressions=misc/valgrind_suppressions.txt --track-fds=yes --read-var-info=yes --gen-suppressions=yes --leak-check=full --vgdb-error=1 ./ohrrpgce-custom $*

# To run helgrind instead, to think multithreading bugs:
# (NOTE: I recommend compiling with debug=1 music=silence gengcc=1 first.
# gengcc=1 is to get line numbers in the valgrind backtraces.)
#valgrind --tool=helgrind --suppressions=misc/valgrind_suppressions.txt --track-fds=yes --read-var-info=yes --gen-suppressions=yes --vgdb-error=1 ./ohrrpgce-custom $*
