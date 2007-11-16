@echo off

cls

if exist "%1.bas" goto st

echo "No such source file"
goto end

:st

fbc -c -lang deprecated %1.bas %2 %3 %4 %5 %6 %7 %8 %9
if ERRORLEVEL 1 goto err

if exist "%1.o" del %1.o

echo Source is good
goto end

:err
:end