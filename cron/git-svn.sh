#!/bin/sh

# This script is run from cron to keep the git-svn mirrors on bitbucket and github up to date.

LOCALREPO=~/ohr/ohrrpgce-git-svn
REMOTEREPO=git@bitbucket.org:rbv/ohrrpgce-svn.git
REMOTEREPO2=git@github.com:ohrrpgce/ohrrpgce.git
SVNREPO=https://rpg.hamsterrepublic.com/source

if [ ! -d "$LOCALREPO" ]; then
    echo "Cloning from $REMOTEREPO"

    git clone "$REMOTEREPO" "$LOCALREPO" --origin svn --no-checkout || exit
    cd "$LOCALREPO"
    # svn/HEAD gets created automatically when cloning, delete it
    git branch -d -r svn/HEAD || exit
    # The bitbucket remote is called 'svn', the github one is called 'gh'
    git remote add gh $REMOTEREPO2
    # Instead of running "git svn init" we set up git-svn our own way
    git config svn-remote.svn.url "$SVNREPO"
    git config svn-remote.svn.fetch "wip:refs/remotes/svn/wip"
    git config svn-remote.svn.fetch "tools:refs/remotes/svn/tools" --add
    git config svn-remote.svn.fetch "web:refs/remotes/svn/web" --add
    git config svn-remote.svn.branches "rel/*:refs/remotes/svn/*"

    echo "Rebuilding svn indexes will take a few minutes!"
fi

cd "$LOCALREPO"

git svn fetch || exit
git push svn refs/remotes/svn/*:refs/heads/*
git push gh refs/remotes/svn/*:refs/heads/*
