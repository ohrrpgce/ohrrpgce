#!/bin/bash

# If you want to test ohrrpgce web builds locally, you can't use file:// urls. Instead you
# should run a local webserver

# This should be the same checked out copy of the ohrrpgce source that you used to build the emscripten web build of the ohrrpgce
export OHRDIR=${OHRDIR:-~/src/ohrrpgce} # /src/ohr

# Stop if any volumes are missing
if [ ! -e "${OHRDIR}" ] ; then echo "Can't mount volume because it does not exist ${OHRDIR}" ; exit 1 ; fi

docker run -it --rm -d -p 8080:80 -v "${OHRDIR}":/usr/share/nginx/html --name ohrrpgce-localweb nginx

echo "Local nginx web server has been started at port 8080"
echo "In your browser, open http://loalhost:8080/ohrrpgce-game.html or http://loalhost:8080/ohrrpgce-custom.html"
echo "To stop the local webserver, press Ctrl+C"

read -r -d '' _

docker stop ohrrpgce-localweb
