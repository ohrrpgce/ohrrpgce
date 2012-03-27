#!/bin/sh

echo "This script installs the fedora packages that are required to"
echo "compile the OHRRPGCE. You must manually install FreeBasic and"
echo "Euphoria because they are not currently available as deb packages"
echo ""
echo "http://freebasic.net/"
echo "http://openeuphoria.org/"
echo "------------------------------------------------------------------"

sudo yum install gcc gcc-c++ scons libX11-devel libXpm-devel libXrandr-devel libXrender-devel SDL-devel SDL_mixer-devel ncurses-devel gdb

# gdb isn't strictly required, but is useful anyway
