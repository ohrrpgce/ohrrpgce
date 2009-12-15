@echo off
if "%1"=="~" shift

if exist "gfx_%1.bas" goto arg1_ok
if "%1"=="directx" goto arg1_ok
if exist "gfx_%OHRGFX%.bas" goto nobackends
if "%OHRGFX%"=="directx" goto nobackends
set OHRGFX=fb
goto nobackends

:arg1_ok
set OHRGFX=%1
set OHRMUSIC=%2
shift
shift

:nobackends

set GFX_XTRA=gfx_%OHRGFX%.bas
if "%OHRGFX%"=="directx" set GFX_XTRA=

if not exist "music_%OHRMUSIC%.bas" set OHRMUSIC=sdl

set MUSIC_XTRA=music_%OHRMUSIC%.bas
if "%OHRMUSIC%"=="sdl" set MUSIC_XTRA=music_sdl.bas sdl_lumprwops.bas
if "%OHRMUSIC%"=="native" set MUSIC_XTRA=music_native.bas -l audwrap -l audiere
if "%OHRMUSIC%"=="native2" set MUSIC_XTRA=music_native2.bas -l audwrap -l audiere

echo Now compiling CUSTOM with %OHRGFX% graphics module, and %OHRMUSIC% music module
call fbc -lang deprecated verprint.bas
verprint %OHRGFX% %OHRMUSIC%
call fbc -lang deprecated -s gui -mt -m custom custom.bas customsubs.bas drawing.bas subs.bas subs2.bas subs3.bas mapsubs.bas flexmenu.bas menus.bas allmodex.bas backends.bas lumpfile.bas compat.bas bam2mid.bas slices.bas sliceedit.bas reload.bas %GFX_XTRA% -l ohrblit %MUSIC_XTRA% loading.bas common.bas browse.bas util.bas cicon.rc -d IS_CUSTOM  %1 %2 %3 %4 %5 %6 %7 %8 %9
echo.
