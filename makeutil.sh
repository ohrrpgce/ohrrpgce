#!/bin/sh
fbc -lang deprecated -v unlump.bas util.bas || exit 1
fbc -lang deprecated -v relump.bas util.bas || exit 1
