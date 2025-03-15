########
# This script does not work yet. It installs the dependencies but you can't compile yet
# Tracking it with https://github.com/ohrrpgce/ohrrpgce/issues/1281
########

The purpose of the macdev.yml ansible playbook is to set up a Mac
with the dependencies needed for OHRRPGCE development, including
everything needed to build nightly builds and stable release builds

Prerequisites
* A Mac with ssh server enabled (System Settings -> Sharing -> Remote Login)
* A user on the mac
* Homebrew should be installed for that user
* Mac command line tools should be installed. (Homebrew installation will prompt for this)
* Set up ssh key authentication set up to connect as that user
* You will need to know your Mac's IP address or hostname
* Make a copy of inventory.yml and customize it for your mac an your user

This does not check out a copy of the OHRRPGCE source code for you,
you can do that yourself with git or svn

Stuff is installed by default to ~/src/ohrmac/

Tested on MacOS Sequoia
