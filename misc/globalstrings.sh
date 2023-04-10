#!/bin/bash
# This script finds all readglobalstring calls, and diffs them against the
# definitions of the global strings in the Global Text Strings menu to find
# inconsistencies or strings which aren't used.

# All unique readglobalstring calls
USES=$(grep -oh "readglobalstring(\([^()]\|([^)]*)\)*)" *.*bas | perl -pe 's~readglobalstring\((.*)\)$~\1~' | sort -n | uniq)

# Get list from Global Text Strings menu
# This converts each line to (offset, default, length), skipping the description and trailing args and comments
DEFNS=$(grep -o "^ add_item [0-9][^']*" globalstredit.bas | perl -e 's/ add_item ([0-9]+), *".*?", *(".*?"), *([0-9]*).*/$1, $2, $3/;' -p | sort -n)

#echo "$USES"
#echo "$DEFNS"

diff <( echo "$USES" ) <( echo "$DEFNS" )
