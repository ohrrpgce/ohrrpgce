@echo off

REM makegame [graphics_backends...] [~] [music_backend] [extra_options]

REM Usage example (gfx_directx, gfx_sdl, music_sdl debugging build):
REM  makegame directx sdl ~ sdl -g

if "%1"=="~" shift

set GFX_XTRAS=
set GFX_MODULES=

:trygfx
if exist "gfx_%1.bas" goto gfxok
if "%1"=="directx" goto gfxok
if "%1"=="sdlpp" goto gfxok
goto endgfxbackends
:gfxok

if not "%GFX_MODULES%"=="" set GFX_MODULES=%GFX_MODULES%+
set GFX_MODULES=%GFX_MODULES%%1
set GFX_XTRA=gfx_%1.bas
if "%1"=="directx" set GFX_XTRA=
if "%1"=="sdlpp" set GFX_XTRA=

set GFX_XTRAS=%GFX_XTRAS% %GFX_XTRA%
shift
goto trygfx

:endgfxbackends
if not "%GFX_MODULES%"=="" goto nogfxgiven
set GFX_XTRAS=gfx_fb.bas gfx_sdl.bas
set GFX_MODULES=directx+sdl+fb
:nogfxgiven
if "%1"=="~" shift

rem echo xtras=%GFX_XTRAS%

set OHRMUSIC=sdl
if not exist "music_%1.bas" goto nomusicgiven
set OHRMUSIC=%1
shift
:nomusicgiven

set MUSIC_XTRA=music_%OHRMUSIC%.bas
if "%OHRMUSIC%"=="sdl" set MUSIC_XTRA=music_sdl.bas sdl_lumprwops.bas
if "%OHRMUSIC%"=="native" set MUSIC_XTRA=music_native.bas win32\audwrap.o -l audiere
if "%OHRMUSIC%"=="native2" set MUSIC_XTRA=music_native2.bas win32\audwrap.o -l audiere

echo Now compiling GAME with %GFX_MODULES% graphics modules, and %OHRMUSIC% music module
call fbc -lang deprecated verprint.bas
verprint %GFX_MODULES% %OHRMUSIC%
call fbc -lang deprecated -s gui -mt -m game game.bas hsinterpreter.bas bmod.bas bmodsubs.bas bcommon.bas allmodex.bas backends.bas lumpfile.bas menustuf.bas moresubs.bas yetmore.bas yetmore2.bas savegame.bas misc.bas bam2mid.bas %GFX_XTRAS% win32\blit.o win32\base64.o %MUSIC_XTRA% loading.bas common.bas browse.bas util.bas slices.bas reload.bas reloadext.bas os_windows.bas gicon.rc -d IS_GAME  %1 %2 %3 %4 %5 %6 %7 %8 %9
echo.
