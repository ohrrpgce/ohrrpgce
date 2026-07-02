#!/bin/sh

# This is a convenience script to get the revision number of the wip branch of the repo
# It also serves to document how this one-liner works. You don't have to call this script.
# You can just use the one-liner directly if you wish.
#
# 18b01b80f is the last commit before the subversion upstream repo was shut down.
# The git repo became the main upstream after that commit.
#
# 14308 was the last SVN revision number. We calculate the revision number now by
# starting with that number, and counting the commits that are present on the
# current HEAD but were absent on commit 18b01b80f
#
# This number is meaningful for wip builds on the wip branch.
# For release branches, the revision number in codename.txt will be used
# (Although this script will still likely produce more-or-less sensible values for release branches)

echo $(( $(git rev-list --count 18b01b80f..HEAD) + 14308 ))

# git rev-list is pretty cool, isn't it? :D
