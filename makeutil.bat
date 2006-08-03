@echo off
echo Now compiling OHRRPGCE utilities
fbc unlump.bas util.bas
fbc relump.bas util.bas
echo.
