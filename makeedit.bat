@echo off
if "%1"=="~" goto noparam
if "%1"=="" goto noparam
if Not exist "gfx_%1.bas" goto noparam2
set OHRGFX=%1
shift
if "%1"=="" goto noparam
if Not exist "music_%1.bas" goto noparam2
set OHRMUSIC=%1

:noparam
shift

:noparam2

IF "%OHRGFX%"=="" set OHRGFX=fb
IF "%OHRMUSIC%"=="" set OHRMUSIC=sdl
if Not exist "gfx_%OHRGFX%.bas" set OHRGFX=fb
if Not exist "music_%OHRMUSIC%.bas" set OHRMUSIC=sdl

if "%OHRGFX%"=="directx" set OHRGFXXTRA=-l gfx_directx gfx_directx.res -d EXTERN_GFX

echo Now compiling CUSTOM with %OHRGFX% graphics module, and %OHRMUSIC% music module
call fbc -lang deprecated verprint.bas
verprint %OHRGFX% %OHRMUSIC%
call fbc -lang deprecated -s gui -mt -m custom custom.bas customsubs.bas drawing.bas subs.bas subs2.bas subs3.bas mapsubs.bas flexmenu.bas menus.bas allmodex.bas lumpfile.bas compat.bas bam2mid.bas slices.bas sliceedit.bas reload.bas %OHRGFXXTRA% gfxsubs.bas gfx_%OHRGFX%.bas music_%OHRMUSIC%.bas loading.bas common.bas browse.bas util.bas cicon.rc -d IS_CUSTOM  %1 %2 %3 %4 %5 %6 %7 %8 %9
echo.
