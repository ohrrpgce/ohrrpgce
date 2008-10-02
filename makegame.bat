@echo off
if "%1"=="~" goto noparam
if "%1"=="" goto noparam
set OHRGFX=%1
shift
if "%1"=="" goto noparam
set OHRMUSIC=%1

:noparam

shift

IF "%OHRGFX%"=="" set OHRGFX=fb
IF "%OHRMUSIC%"=="" set OHRMUSIC=sdl
if Not exist "gfx_%OHRGFX%.bas" set OHRGFX=fb
if Not exist "music_%OHRMUSIC%.bas" set OHRMUSIC=sdl

echo Now compiling GAME with %OHRGFX% graphics module, and %OHRMUSIC% music module
call fbc -lang deprecated verprint.bas
verprint %OHRGFX% %OHRMUSIC%
call fbc -lang deprecated -s gui -mt -m game game.bas bmod.bas bmodsubs.bas allmodex.bas menustuf.bas moresubs.bas yetmore.bas yetmore2.bas compat.bas bam2mid.bas gfx_%OHRGFX%.bas music_%OHRMUSIC%.bas loading.bas common.bas browse.bas util.bas slices.bas gicon.rc -d IS_GAME  %1 %2 %3 %4 %5
echo.
