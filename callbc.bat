IF NOT EXIST %QBDIR%\bc.exe GOTO NOBC
echo %1
echo %1 >>compile.out
%QBDIR%\bc.exe %OHRRPGCE%\%1.bas/O/AH/T/E/C:1;>>compile.out
GOTO FINISH

:NOBC
ECHO BC.EXE Quickbasic 4.5 Compiler not found at %QBDIR%
ECHO Please read README.TXT

:FINISH