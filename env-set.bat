@ECHO OFF

REM --- Edit this file to select the locations ---
REM --- where you have installed Quickbasic,   ---
REM --- and the OHRRPGCE source code.          ---

SET QBDIR=C:\QB45
SET OHRRPGCE=C:\OHRRPGCE

ECHO ####################
ECHO #  O.H.R.RPG.C.E.  #
ECHO ####################

ECHO Expecting to find Quickbasic 4.5 in %QBDIR%
ECHO Expecting to find OHRRPGCE sources in %OHRRPGCE%
ECHO If those are reasonable assumptions, you are ready to go!
ECHO.
ECHO   q game        Edit the GAME source code
ECHO   q custom      Edit the CUSTOM source code
ECHO   compile       Compile both GAME and CUSTOM to EXE files
ECHO.