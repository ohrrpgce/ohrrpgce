#!/bin/sh

# On one or two occasions, when CastleParadox has gone down, Inferior Minion
# Has redirected all traffic to the mirror. This script calculates a table
# that maps CP game ID numbers to mirror filenames so that direct links to
# download games will do-the-right-thing in spite of the differing layout of
# the mirror.

echo "<?php"

for FILE in gamelist-display-*.html ; do
  Mirror_ID=`echo $FILE | sed s/"^gamelist-display-\([0-9]\+\)\.html$"/"\1"/`
  CP_ID=`grep -e "Mirrored from castleparadox\.com/gamelist-display\.php?game=" ${FILE} \
          | head -1 \
          | cut -d " " -f 4 \
          | cut -d "=" -f 2 \
          | cut -d "&" -f 1 `
  DOWNLOAD=`grep -e "?game=${CP_ID}\">Download:" "${FILE}" \
          | cut -d \" -f 8 \
          | cut -d "?" -f 1`
  printf "\$map[%d] = %d; " "${CP_ID}" "${Mirror_ID}"
  printf "\$dl[%d] = \"%s\";\n" "${CP_ID}" "${DOWNLOAD}"
done

echo "?>"
