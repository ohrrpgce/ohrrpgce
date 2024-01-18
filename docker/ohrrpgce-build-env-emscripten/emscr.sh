#!/bin/bash

# Run this script as your regular user. The same user who has access to the ohrrpgce source dir
# Don't use root.

# Note: to test emscripten web builds in your browser, you can't use file:// urls
# instead run: python3 -m http.server
# inside the ohrrpgce source dir, and then browse to http://localhost:8000/

SCRIPT_DIR="${0%/*}"
SCRIPT_DIR="$(realpath $SCRIPT_DIR)"
cd "${SCRIPT_DIR}"

#-----------------------------------------------------------------------
# Config

# Paths on the host that will be mounted as volumes inside the docker container

# This should be a checked out copy of the ohrrpgce source
export OHRDIR=${OHRDIR:-~/src/ohrrpgce} # /src/ohr
echo "OHRDIR=${OHRDIR}"

# The docker image name. This will be built locally, we won't pull it
export ANDRIMG=bobthehamster/ohrrpgce-build-env-emscripten

#-----------------------------------------------------------------------
# Command line arguments

export RUNCMD=""
export REBUILD_IMAGE="Y"
export INTERACTIVE_TERMINAL="-it"

POSITIONAL_ARGS=()

while [[ $# -gt 0 ]]; do
  case $1 in
    -h|--help)
      cat <<EOF
Build and run an OHRRPGCE Emscripten build environment in a docker image.

  -c  --run-command 'command arg arg'  Run this quoted command in the
                                       image, instead of an interactive prompt
  
  -sb --skip-build-image       Don't try to rebuild the docker image
EOF
      exit 0
      ;;
    -c|--run-cmd)
      export RUNCMD="$2"
      export INTERACTIVE_TERMINAL=""
      shift
      shift
      ;;
    -sb|--skip-build-image)
      export REBUILD_IMAGE="N"
      shift
      ;;
    -*|--*)
      echo "Unknown option $1"
      exit 1
      ;;
    *)
      POSITIONAL_ARGS+=("$1") # save positional arg
      shift # past argument
      ;;
  esac
done

set -- "${POSITIONAL_ARGS[@]}" # restore positional args

echo "REBUILD_IMAGE=$REBUILD_IMAGE"

#-----------------------------------------------------------------------

if [ -z "$(docker images -q ${ANDRIMG})" -o "$REBUILD_IMAGE" = "Y" ] ; then
  # Rebuild the docker image each time. Skip if we requested to skip,
  # unless the image doesn't exist at all, in which case we can't skip it.
  echo "Build the docker image"
  echo "(This will be super slow the first time)"
  docker build -f "Dockerfile" --tag="${ANDRIMG}" "${SCRIPT_DIR}"
  EXITCODE="$?"
  if [ "$EXITCODE" -ne 0 ] ; then
    echo "Docker build failed with exit code $EXITCODE"
    exit 1
  fi
fi

if [ -s "$RUNCMD" ] ; then
  export HASCMD="-c"
fi

# Stop if any volumes are missing
if [ ! -e "${OHRDIR}" ] ; then echo "Can't mount volume because it does not exist ${OHRDIR}" ; exit 1 ; fi

echo "Now run a docker shell into the emscripten container with OHRRPGCE source mounted"
echo "Running as user $(whoami) UID:GID=$(id -u):$(id -g) which will be \"I have no name!\" inside the container,"
echo "but don't worry, it will still be correct for volume mounts."
docker run --rm ${INTERACTIVE_TERMINAL} \
  -v "${OHRDIR}":/src/ohr \
  -u $(id -u):$(id -g) \
  "${ANDRIMG}" /bin/bash $HASCMD $RUNCMD
