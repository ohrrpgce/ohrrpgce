#!/bin/bash
# This script finds all readglobalstring calls, and diffs them against the
# definitions of the global strings in the Global Text Strings menu to find
# inconsistencies or strings which aren't used.

# All unique readglobalstring calls
USES=$(grep -oh "readglobalstring(\([^()]\|([^)]*)\)*)" *.*bas | perl -pe 's~readglobalstring\((.*)\)$~\1~' | sort -n | uniq)

# Get list from Global Text Strings menu
# This strips out the description of the global string, and trailing comments and whitespace
DEFNS=$(grep -o "GTS_add_to_menu menu, [^']*" *.*bas | perl -e 's/^.*?".*?".*?, *//;' -pe 's/ +$//' | sort -n)

#echo "$USES"
#echo "$DEFNS"

diff <( echo "$USES" ) <( echo "$DEFNS" )
