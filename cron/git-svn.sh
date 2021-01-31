#!/bin/sh

# This script is run from cron to keep the git-svn mirrors on bitbucket and github up to date.

LOCALREPO=~/ohr/ohrrpgce-git-svn
SVNREPO=https://rpg.hamsterrepublic.com/source
# These git repos receive wip/, tools/,  and branches
REMOTEREPO=git@bitbucket.org:rbv/ohrrpgce-svn.git
REMOTEREPO2=git@github.com:ohrrpgce/ohrrpgce.git
# This git repo receives web/
REMOTEREPO_WEB=git@github.com:ohrrpgce/web.git
# This git repo receives tools/
REMOTEREPO_TOOLS=git@github.com:ohrrpgce/tools.git

if [ ! -d "$LOCALREPO" ]; then
    echo "Cloning from $REMOTEREPO"

    git clone "$REMOTEREPO" "$LOCALREPO" --origin svn --no-checkout || exit
    cd "$LOCALREPO"
    # svn/HEAD gets created automatically when cloning, delete it
    git branch -d -r svn/HEAD || exit
    # The bitbucket remote is called 'svn', the github one is called 'gh'
    git remote add gh $REMOTEREPO2
    git remote add web $REMOTEREPO_WEB
    git remote add tools $REMOTEREPO_TOOLS
    # Instead of running "git svn init" we set up git-svn our own way
    git config svn-remote.svn.url "$SVNREPO"
    git config svn-remote.svn.fetch "wip:refs/remotes/svn/wip"
    git config svn-remote.svn.fetch "tools:refs/remotes/svn/tools" --add
    git config svn-remote.svn.fetch "web:refs/remotes/svn/web" --add
    git config svn-remote.svn.branches "rel/*:refs/remotes/svn/*"

    # git config branch.web.remote web
    # git config branch.web.merge master
    # git config branch.tools.remote tools
    # git config branch.tools.merge master

    echo "Rebuilding svn indexes will take a few minutes!"
fi

cd "$LOCALREPO"

# NOTE: for some reason rel/ branches may be missing from refs/remotes/svn
# except for those added/changed since first fetching from svn although they are
# in .git/svn/refs/remotes/svn

# Get list of refspecs (branches) to push to main repos
REFS=
cd .git/refs/remotes/svn/
for ref in *; do
    if [ ! "$ref" = "web" ] && [ ! "$ref" = "tools" ]; then
        REFS="$REFS refs/remotes/svn/$ref:refs/heads/$ref"
    fi
done

git svn fetch || exit
git push svn $REFS
git push gh $REFS
git push tools refs/remotes/svn/tools:refs/heads/tools
git push web refs/remotes/svn/web:refs/heads/web
