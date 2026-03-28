# ABOUT THE DOCKER IMAGES IN THIS FOLDER

See https://rpg.hamsterrepublic.com/ohrrpgce/Compiling_with_Docker for more details.

## Stable
ohrrpgce-build-env-emscripten
ohrrpgce-build-env-linux-x86
ohrrpgce-build-env-linux-x86_64

Run the bash script inside these folders. It will build the Docker image
and run the container with your $OHRDIR folder mounted to /src/ohr

## Unstable 
ohrrpgce-build-env-android

This one technically sort-of works for James, but it requires a lot of manual config,
plus it needs an ancient SDK version from 2012 and the apk it procudes does not work on Android >= 12

## Obsolete/Experimental

All other subfolders should be considered either obsolete or experimental.
They might not work, they might be deleted, or they migh change very much
before they become useful. You can ignore them.
