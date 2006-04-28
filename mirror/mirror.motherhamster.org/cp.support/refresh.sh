#!/bin/sh

cd ~/mirror.motherhamster.org/cp
ln -s ../cp.support/cp.htaccess ./.htaccess
ln -s ../cp.support/404.php ./404.php
cd castleparadox.com
../../cp.support/cp-id.sh > ../id-map.php
