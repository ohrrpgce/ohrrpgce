#!/bin/bash

# Run this script as your regular user. The same user who has access to the ohrrpgce source dir
# Don't use root.

# Note this isn't really a 32 bit Linux,
# it is just a 64 bit Linux configured to use 32 bit build libs for the OHRRPGCE

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
export DOCKERIMG=bobthehamster/ohrrpgce-build-env-linux-x86

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
Build and run an OHRRPGCE 32 bit Linux build environment in a docker image.

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

if [ -z "$(docker images -q ${DOCKERIMG})" -o "$REBUILD_IMAGE" = "Y" ] ; then
  # Rebuild the docker image each time. Skip if we requested to skip,
  # unless the image doesn't exist at all, in which case we can't skip it.
  echo "Build the docker image"
  echo "(This will be super slow the first time)"
  docker build -f "Dockerfile" --tag="${DOCKERIMG}" "${SCRIPT_DIR}"
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

echo "Now run a docker shell into the Linux container with OHRRPGCE source mounted"
echo "Running as user $(whoami) UID:GID=$(id -u):$(id -g) which will be \"I have no name!\" inside the container,"
echo "but don't worry, it will still be correct for volume mounts."
echo "----------------------------------------------------------------------------------"
echo "Welcome to OHRRPGCE linux 32bit build env."
echo "Source is mounted at /src/ohr and you can try running one of these in that folder"
echo "  scons arch=x86"
echo "  ./distrib-linux.sh"
docker run --rm ${INTERACTIVE_TERMINAL} \
  -v "${OHRDIR}":/src/ohr \
  -u $(id -u):$(id -g) \
  "${DOCKERIMG}" /bin/bash $HASCMD $RUNCMD
