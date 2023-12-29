#!/bin/bash

# For now this script does need to do anything differently than distrib-web.sh
# except for outputting zip files with no dates. It is okay if that has to change later

SCRIPTDIR="${0%/*}"
SCRIPTDIR="$(realpath $SCRIPTDIR)"
cd "${SCRIPTDIR}"

./distrib-web.sh "ohrrpgce-web-{BRANCH}" "ohrrpgce-player-web-{BRANCH}"
