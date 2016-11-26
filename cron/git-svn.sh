#!/bin/sh

# TMC runs this script from cron to keep the git-svn mirror up to date.

LOCALREPO=~/src/ohrrpgce-svn
REMOTEREPO=git@bitbucket.org:rbv/ohrrpgce-svn.git
SVNREPO=https://rpg.hamsterrepublic.com/source

if [ ! -d "$LOCALREPO" ]; then
    echo "Cloning from $REMOTEREPO"

    git clone "$REMOTEREPO" "$LOCALREPO" --origin svn
    # svn/HEAD gets created automatically when cloning, delete it
    git branch -d -r svn/HEAD
    cd "$LOCALREPO"
    git config svn-remote.svn.url "$SVNREPO"
    git config svn-remote.svn.fetch "wip:refs/remotes/svn/wip"
    git config svn-remote.svn.fetch "tools:refs/remotes/svn/tools" --add
    git config svn-remote.svn.fetch "web:refs/remotes/svn/web" --add
    git config svn-remote.svn.branches "rel/*:refs/remotes/svn/*"
fi

cd "$LOCALREPO"

git svn fetch &&
git push svn refs/remotes/svn/*:refs/heads/*
