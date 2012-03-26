#!/bin/sh

echo "This script installs the debian packages that are required to"
echo "compile the OHRRPGCE. You must manually install FreeBasic and"
echo "Euphoria because they are not currently available as deb packages"

sudo apt-get install scons libx11-dev libxpm-dev libxrandr-dev libxrender-dev libsdl-mixer1.2-dev timidity
