#!/bin/bash

# NOTE this doesn't work yet! -- 2023-12 James Paige

# Run this script as your regular user. The same user who has access to the ohrrpgce source dir
# Don't use root.

export SCRIPT_DIR=$(dirname "$0")

#-----------------------------------------------------------------------
# Config

# Paths on the host that will be mounted as volumes inside the docker container
export SDKVOL=${SDKVOL:-~/src/docker-managed-android-sdk} # /opt/android-sdk
export OHRDIR=${OHRDIR:-~/src/ohrrpgce} # /src/ohr
export SDLA=${SDLA:-~/src/sdl-android} # /src/sdl-android

# Ubuntu 22.04 (jammy) ancestor
export ANDRIMG=bobthehamster/ohrrpgce-build-env-android

#-----------------------------------------------------------------------
# Command line arguments

export REBUILD_IMAGE="Y"
export COPY_SDK="N"
export UPDATE_SDK="N"

POSITIONAL_ARGS=()

while [[ $# -gt 0 ]]; do
  case $1 in
    -h|--help)
      cat <<EOF
Build and run an OHRRPGCE Android build environment in a docker image.

  -sb --skip-build-image     Don't try to rebuild the docker image

  -cs --copy-sdk             Re-copy the SDK to the SDK volume. This
                             resets and updates or customizations

  -ua --update-android-sdk   Try to update the android SDK
EOF
      exit 0
      ;;
    -sb|--skip-build-image)
      export REBUILD_IMAGE="N"
      shift
      ;;
    -cs|--copy-sdk)
     export COPY_SDK="Y"
     shift
     ;;
    -ua|--update-android-sdk)
      export UPDATE_SDK="Y"
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
echo "COPY_SDK=$COPY_SDK"
echo "UPDATE_SDK=$UPDATE_SDK"

#-----------------------------------------------------------------------

if [ -z "$(docker images -q ${ANDRIMG})" -o "$REBUILD_IMAGE" = "Y" ] ; then
  # Rebuild the docker image each time. Skip if we requested to skip,
  # unless the image doesn't exist at all, in which case we can't skip it.
  echo "Build the docker image"
  echo "(This will be super slow the first time)"
  docker build -f "next.Dockerfile" --tag="${ANDRIMG}" "${SCRIPT_DIR}"
  EXITCODE="$?"
  if [ "$EXITCODE" -ne 0 ] ; then
    echo "Docker build failed with exit code $EXITCODE"
    exit 1
  fi
fi

if [ ! -e "${SDKVOL}" -o "$COPY_SDK" = "Y" ] ; then
  # Copy the Android SDK to a volume to make it accessible outside the container
  echo "Set up ${SDKVOL}"
  docker run -it --rm -v "${SDKVOL}":/sdk "${ANDRIMG}" \
    bash -c 'cp -a $ANDROID_HOME/. /sdk'
  export UPDATE_SDK="Y"
fi

if [ "$UPDATE_SDK" = "Y" ] ; then
  # Update the copy of the Android SDK in the mounted volume
  echo "Now try to update Android SDK..."
  docker run -it -v "${SDKVOL}":/opt/android-sdk "${ANDRIMG}" \
    bash -c '/opt/android-sdk/cmdline-tools/tools/bin/sdkmanager --update'
  docker run -it -v "${SDKVOL}":/opt/android-sdk "${ANDRIMG}" \
    bash -c '/opt/android-sdk/cmdline-tools/tools/bin/sdkmanager "platform-tools" "build-tools;31.0.0" "platforms;android-31" "ndk;26.1.10909125"'
fi

echo "Now run a docker shell into the android-sdk container with OHRRPGCE source mounted"
echo "Running as user $(whoami) UID:GID=$(id -u):$(id -g) which will be unknown inside the container, but correct for volume mounts."
docker run -it \
  -v "${SDKVOL}":/opt/android-sdk:ro \
  -v "${OHRDIR}":/src/ohr \
  -v "${SDLA}":/src/sdl-android \
  -u $(id -u):$(id -g) \
  "${ANDRIMG}" /bin/bash
