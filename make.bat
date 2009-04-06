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

call makegame %1 %2 %3 %4 %5 %6 %7 %8 %9
call makeedit %1 %2 %3 %4 %5 %6 %7 %8 %9
