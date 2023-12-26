#!/bin/bash

# This is to be run inside the docker comntainer to fix the symlinks
# inside /src/sdl-android .. Which would mess up the symlinks outside
# the docker image, but we are assuming this volume is only used inside
# the container.

rm -f /src/sdl-android/project/jni/application/ohrrpgce
ln -s /src/ohr/android /src/sdl-android/project/jni/application/ohrrpgce

rm -f /src/sdl-android/project/jni/freebasic/rtlib
ln -s /src/fbc-arm-rtlib /src/sdl-android/project/jni/freebasic/rtlib
