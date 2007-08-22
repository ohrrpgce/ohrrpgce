@echo off
echo Now compiling OHRRPGCE utilities
fbc -lang deprecated unlump.bas util.bas
fbc -lang deprecated relump.bas util.bas
echo.
