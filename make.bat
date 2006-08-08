@echo off
if "%1"=="" goto gen
if Not exist "gfx_%1" goto def

pause

call makeedit-gfx-music.bat %1 %2 %3 %4 %5 %6 %7
call makegame-gfx-music.bat %1 %2 %3 %4 %5 %6 %7
goto end

:def

call makegame %1 %2 %3 %4 %5
call makeedit %1 %2 %3 %4 %5
goto end

:gen

call makegame
call makeedit
goto end

:end

