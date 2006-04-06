@echo off
if "%1"=="" goto gen
pause

call makeedit-gfx-music.bat %1 %2
call makegame-gfx-music.bat %1 %2
goto end

:gen

call makegame
call makeedit

:end

