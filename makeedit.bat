@echo off
IF "%OHRGFX%"=="" set OHRGFX=fb
IF "%OHRMUSIC%"=="" set OHRMUSIC=native
makeedit-gfx-music.bat %OHRGFX% %OHRMUSIC% %1 %2 %3 %4 %5
