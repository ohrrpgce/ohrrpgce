@ECHO OFF

REM --- Edit this file to select the locations ---
REM --- where you have installed Quickbasic,   ---
REM --- and the OHRRPGCE source code.          ---

REM -- Quickbasic 4.5 is the default --
REM SET QBVER=4.5
REM SET QBDIR=C:\QB45
REM SET OHRRPGCE=C:\OHRRPGCE
REM SET QBEDIT=QB.EXE
REM SET LINKEXT=l

REM -- Quickbasic 7.1 also works --
SET QBVER=7.1
SET QBDIR=C:\QBX\BINB
SET OHRRPGCE=C:\OHRRPGCE\SOURCE\CURRENT
SET QBEDIT=QBX.EXE
SET LINKEXT=l7

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
