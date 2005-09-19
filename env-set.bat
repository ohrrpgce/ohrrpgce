@ECHO OFF

REM   The PATH is the only bit needed for FreeBasic, I left the rest so
REM   this can be used for the QB version, too, and also because I am
REM   shockingly lazy.

REM --- Edit this file to select the locations ---
REM --- where you have installed Quickbasic,   ---
REM --- and the OHRRPGCE source code.          ---

SET PATH=C:\WINDOWS\COMMAND;C:\WINDOWS;C:\DOS;E:\SUBVERSION\BIN;E:\FREEBASIC\;e:\freebasic\bin\win32

REM -- Quickbasic 4.5 is the default --
SET QBVER=4.5
SET QBDIR=C:\DOS
SET OHRRPGCE=E:\dev\ohr\OHRRPGCE
SET QBEDIT=QB.EXE
SET LINKEXT=l

REM -- Quickbasic 7.1 also works --
REM SET QBVER=7.1
REM SET QBDIR=C:\QB71
REM SET OHRRPGCE=C:\OHRRPGCE
REM SET QBEDIT=QBX.EXE
REM SET LINKEXT=l7

REM Attention Windows XP users! You must define these five
REM environment variables manually. This batch file will not
REM work. Right-click on "My Computer", pick "Properties", go
REM to the "Advanced" tab, and click "Environment Variables"


ECHO ####################
ECHO #  O.H.R.RPG.C.E.  #
ECHO ####################

ECHO Expecting to find OHRRPGCE sources in %OHRRPGCE%
ECHO Expecting to find Quickbasic %QBVER% in %QBDIR%
ECHO Expecting Quickbasic editor to be %QBEDIT%
ECHO If those are reasonable assumptions, you are ready to go!
ECHO.
ECHO   q game        Edit the GAME source code
ECHO   q custom      Edit the CUSTOM source code
ECHO   compile       Compile both GAME and CUSTOM to EXE files
ECHO.

doskey
