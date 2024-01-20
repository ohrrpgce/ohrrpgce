#!/bin/bash

# Run this script as your regular user. The same user who has access to the ohrrpgce source dir
# Don't use root.

SCRIPT_DIR="${0%/*}"
SCRIPT_DIR="$(realpath $SCRIPT_DIR)"
cd "${SCRIPT_DIR}"

#-----------------------------------------------------------------------
# Config

# Paths on the host that will be mounted as volumes inside the docker container

# These are from 2012, and I don't even know how to check the exact versions
# They only exist on James's computer (We will keep trying to switch to newer ones)
export OLDSDKVOL=${OLDSDKVOL:-~/misc/android-sdk-linux_x86.old} # /opt/android-sdk
export OLDNDKVOL=${OLDNDKVOL:-~/misc/android-ndk-r12b} # /opt/android-ndk

# This is where the debug signing key will be stored
export DOTANDROIDVOL=${DOTANDROIDVOL:-~/misc/docker-dot-android.old}

# This should be a checked out copy of the ohrrpgce source
export OHRDIR=${OHRDIR:-~/src/ohrrpgce} # /src/ohr

# This should be a checked out copy of
# https://github.com/bob-the-hamster/commandergenius
export SDLA=${SDLA:-~/src/sdl-android} # /src/sdl-android

# The docker image name. This will be built locally, we won't pull it
export ANDRIMG=bobthehamster/ohrrpgce-build-env-android-oldstyle

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
Build and run an OHRRPGCE Android build environment in a docker image.

  -c  --run-command 'command arg arg'  Run this quoted command in the
                                       image, instead of an interactive prompt
  
  -sb --skip-build-image       Don't try to rebuild the docker image
  
  -gp --gameproject            The name of an ohrrpgce android game project
                               which will be mounted as a volume and built.
                               Note that the symlinks inside this directory
                               will be updated to make sense for a dockerized
                               build
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
    -gp|--gameproject)
      export GAMEPROJ="$2"
      shift
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

if [ -n "${RUNCMD}" -a -n "${GAMEPROJ}" ] ; then
  echo "The --run-command and --gameproject arguments cannot be used at the same time (-c -gp)"
  exit 1
fi

echo "REBUILD_IMAGE=$REBUILD_IMAGE"

#-----------------------------------------------------------------------

if [ -z "$(docker images -q ${ANDRIMG})" -o "$REBUILD_IMAGE" = "Y" ] ; then
  # Rebuild the docker image each time. Skip if we requested to skip,
  # unless the image doesn't exist at all, in which case we can't skip it.
  echo "Build the docker image"
  echo "(This will be super slow the first time)"
  docker build -f "oldstyle.Dockerfile" --tag="${ANDRIMG}" "${SCRIPT_DIR}"
  EXITCODE="$?"
  if [ "$EXITCODE" -ne 0 ] ; then
    echo "Docker build failed with exit code $EXITCODE"
    exit 1
  fi
fi

if [ -s "$RUNCMD" ] ; then
  export HASCMD="-c"
fi

echo "OLDSDKVOL=${OLDSDKVOL}"
echo "OLDNDKVOL=${OLDNDKVOL}"
echo "DOTANDROIDVOL=${DOTANDROIDVOL}"
echo "OHRDIR=${OHRDIR}"
echo "SDLA=${SDLA}"
echo "GAMEPROJ=${GAMEPROJ}"

# Stop if any volumes are missing
if [ ! -e "${OLDSDKVOL}" ] ; then echo "Can't mount volume because it does not exist ${OLDSDKVOL}" ; exit 1 ; fi
if [ ! -e "${OLDNDKVOL}" ] ; then echo "Can't mount volume because it does not exist ${OLDNDKVOL}" ; exit 1 ; fi
if [ ! -e "${DOTANDROIDVOL}" ] ; then echo "Can't mount volume because it does not exist ${DOTANDROIDVOL}" ; exit 1 ; fi
if [ ! -e "${OHRDIR}" ] ; then echo "Can't mount volume because it does not exist ${OHRDIR}" ; exit 1 ; fi
if [ ! -e "${SDLA}" ] ; then echo "Can't mount volume because it does not exist ${SDLA}" ; exit 1 ; fi

if [ -n "${GAMEPROJ}" ] ; then
  if [ ! -e "${GAMEPROJ}" ] ; then echo "Can't mount game project volume because it does not exist ${OLDSDKVOL}" ; exit 1 ; fi
  export GAMENAME=$(basename "${GAMEPROJ}")
  export GAMEPROJ_MOUNT="-v ${GAMEPROJ}:/src/${GAMENAME}"
  echo "Modifying the symlinks in ${GAMEPROJ} to work inside the docker container"
  echo "You should expect those links to look broken outside the docker container"
  rm -f "${GAMEPROJ}/AndroidAppSettings.cfg"
  ln -s /src/ohr/android/AndroidAppSettings.cfg "${GAMEPROJ}/AndroidAppSettings.cfg"
  rm -f "${GAMEPROJ}/extraconfig.cfg"
  ln -s /src/ohr/android/extraconfig.cfg "${GAMEPROJ}/extraconfig.cfg"
  rm -f "${GAMEPROJ}/tmp"
  ln -s /src/ohr/android/tmp "${GAMEPROJ}/tmp"
  rm -f "${SDLA}/project/jni/application/${GAMENAME}"
  ln -s "/src/${GAMENAME}" "${SDLA}/project/jni/application/${GAMENAME}"
fi

echo "Now run a docker shell into the android-sdk container with OHRRPGCE source mounted"
echo "Running as user $(whoami) UID:GID=$(id -u):$(id -g) which will be \"I have no name!\" inside the container,"
echo "but don't worry, it will still be correct for volume mounts."
docker run --rm ${INTERACTIVE_TERMINAL} \
  -v "${OLDSDKVOL}":/opt/android-sdk:ro \
  -v "${OLDNDKVOL}":/opt/android-ndk-r12b:ro \
  -v "${DOTANDROIDVOL}":/.android \
  -v "${OHRDIR}":/src/ohr \
  -v "${SDLA}":/src/sdl-android \
  ${GAMEPROJ_MOUNT} \
  -u $(id -u):$(id -g) \
  "${ANDRIMG}" /bin/bash $HASCMD $RUNCMD
