@echo off
IF "%OHRGFX%"=="" set OHRGFX=fb
IF "%OHRMUSIC%"=="" set OHRMUSIC=sdl
makeedit-gfx-music.bat %OHRGFX% %OHRMUSIC% %1 %2 %3 %4 %5
