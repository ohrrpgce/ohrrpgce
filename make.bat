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
if Not exist "gfx_%OHRGFX%" set OHRGFX=fb
if Not exist "music_%OHRMUSIC%" set OHRMUSIC=sdl

echo OHRGFX=%OHRGFX%
echo OHRMUSIC=%OHRMUSIC%
pause

call makegame %1 %2 %3 %4 %5
call makeedit %1 %2 %3 %4 %5


