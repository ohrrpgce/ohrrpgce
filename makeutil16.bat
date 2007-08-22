@echo off
REM This script is for FreeBasic version 0.16b and older
echo Now compiling OHRRPGCE utilities
fbc unlump.bas util.bas
fbc relump.bas util.bas
echo.
