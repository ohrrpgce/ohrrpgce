#!/bin/sh
# Push a commandline executable to an android emulator and run it under gdb.
# See also compile-using-toolchain.sh
#
# This file is based on ndk-gdb from the Android NDK (which does the same
# thing for .apks):
#
# Copyright (C) 2010 The Android Open Source Project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#


# Run compile-using-toolchain.sh to generate this, or you can probably
# point it inside the NDK without needing a standalone toolchain.
#TOOLCHAIN=~/local/android-toolchain-r8
TOOLCHAIN=~/local/android-toolchain

NDK=/opt/android-ndk-r12b
#NDK=/opt/android-ndk-r8e

# For new NDKs, e.g. r12
OLDNDK=
# For older NDKs, e.g. r8
#OLDNDK=YES

# DESTDIR is the location on the device/emulator where files will be pushed to
DESTDIR=/data/HWUserData/freebasic
#DESTDIR=/storage/freebasic

DEBUG_PORT=5011
ADB_FLAGS=
GDBTEMP=gdbtemp

########## End of config

if [ $# -ne 1 ]; then
    echo "Usage:  $0 [binary-to-push]"
    echo "Also, you should edit the config variables at the top of this file."
    exit 1
fi
PROG=$1

if [ $OLDNDK ]; then
    GDBCLIENT=$TOOLCHAIN/bin/arm-linux-androideabi-gdb
else
    GDBCLIENT=$TOOLCHAIN/bin/gdb-orig
fi

if [ ! -x $GDBCLIENT ]; then
    echo "gdb client missing; please edit the config variables at the top of this file."
    exit 1
fi

# If the device doesn't already have gdbserver, we can push it too
# (but I think the emulator images at least always have it?)
#GDBSERVER=$NDK/prebuilt/android-arm/gdbserver/gdbserver
REMOTE_GDBSERVER=gdbserver

mkdir -p $GDBTEMP

log ()
{
    echo "$@"
}

# Run an ADB command with the right ADB flags
# $1+: adb command parameter
adb_cmd ()
{
    adb $ADB_FLAGS "$@"
}

adb shell mkdir -p $DESTDIR

# Return the PID of a given package or program, or 0 if it doesn't run
# $1: Package name ("com.example.hellojni") or program name ("/lib/gdbserver")
# Out: PID number, or 0 if not running
get_pid_of ()
{
    adb_cmd shell ps | awk -f extract-pid.awk -v PACKAGE="$1"
}

log "Pushing $PROG"
adb push $PROG $DESTDIR/

# Check that there is no other instance of gdbserver running
GDBSERVER_PID=$(get_pid_of gdbserver)
if [ "$GDBSERVER_PID" != "0" ]; then
    if [ "$OPTION_FORCE" = "no" ] ; then
        echo "ERROR: Another debug session running, Use --force to kill it."
        exit 1
    fi
    echo
    log "Killing existing debugging session"
    adb_cmd shell kill -9 $GDBSERVER_PID
    echo
fi

# Older gdbservers are broken. Push a modern one.
# adb_cmd push $GDBSERVER $DESTDIR
# REMOTE_GDBSERVER=$DESTDIR/gdbserver

# Launch gdbserver now
#adb_cmd shell run-as $PACKAGE_NAME lib/gdbserver +$DEBUG_SOCKET --attach $PID &
echo
adb_cmd shell $REMOTE_GDBSERVER :$DEBUG_PORT $DESTDIR/$PROG &
if [ $? != 0 ] ; then
    echo "ERROR: Could not launch gdbserver on the device?"
    exit 1
fi
log "Launched gdbserver succesfully."
sleep 0.5

adb_cmd forward tcp:$DEBUG_PORT tcp:$DEBUG_PORT
if [ $? != 0 ] ; then
    echo "ERROR: Could not setup network redirection to gdbserver?"
    echo "       Maybe using --port=<port> to use a different TCP port might help?"
    exit 1
fi

GDBSETUP=$GDBTEMP/gdb.setup
echo "file $PROG" > $GDBSETUP
echo "target remote :$DEBUG_PORT" >> $GDBSETUP

if [ $OLDNDK ]; then
    # Get binaries from the device
    adb_cmd pull /system/bin/linker $GDBTEMP/linker
    log "Pulled linker from device/emulator."
    
    adb_cmd pull /system/lib/libc.so $GDBTEMP/libc.so
    log "Pulled libc.so from device/emulator."

    echo "set solib-search-path $GDBTEMP" >> $GDBSETUP
fi

#$GDBCLIENT -x `native_path $GDBSETUP`
$GDBCLIENT -x $GDBSETUP
rm -rf $GDBTEMP
