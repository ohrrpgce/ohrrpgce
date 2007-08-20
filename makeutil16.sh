#!/bin/sh
fbc -v unlump.bas util.bas || exit 1
fbc -v relump.bas util.bas || exit 1
