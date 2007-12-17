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

echo Now compiling CUSTOM with %OHRGFX% graphics module, and %OHRMUSIC% music module
fbc -lang deprecated verprint.bas
verprint %OHRGFX% %OHRMUSIC%
fbc -lang deprecated -s gui -m custom custom.bas customsubs.bas drawing.bas subs.bas subs2.bas subs3.bas mapsubs.bas flexmenu.bas menus.bas allmodex.bas compat.bas bam2mid.bas gfx_%OHRGFX%.bas music_%OHRMUSIC%.bas loading.bas common.bas browse.bas util.bas cicon.rc -d IS_CUSTOM  %1 %2 %3 %4 %5
echo.
