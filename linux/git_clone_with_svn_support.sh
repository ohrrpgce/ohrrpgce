#!/bin/sh

# This script is how James sets up a git repo with a git-svn upstream
# bridge. For most developers who only care about compling the code
# or making pull requests, all you need is:
#
# git clone https://github.com/ohrrpgce/ohrrpgce.git

SRC_DIR=$HOME/src
OHR_GIT_DIR=$SRC_DIR/ohrrpgce

mkdir -p "$SRC_DIR"

if [ -d "$OHR_GIT_DIR" ] ; then
  echo "${OHR_GIT_DIR} already exists."
else
  git clone https://github.com/ohrrpgce/ohrrpgce.git "$OHR_GIT_DIR" --origin svn
  cd "$OHR_GIT_DIR"
  git remote add origin https://github.com/bob_the_hamster/ohrrpgce.git
  git fetch --all
  git branch -d -r svn/HEAD 
  git config svn-remote.svn.url https://rpg.hamsterrepublic.com/source
  git config svn-remote.svn.fetch "wip:refs/remotes/svn/wip"
  git config svn-remote.svn.fetch "tools:refs/remotes/svn/tools" --add
  git config svn-remote.svn.fetch "web:refs/remotes/svn/web" --add
  git config svn-remote.svn.branches "rel/*:refs/remotes/svn/*"
  git svn fetch --log-window-size=10000
fi



