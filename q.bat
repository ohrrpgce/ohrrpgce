IF NOT EXIST %QBDIR%\qb.exe GOTO NOQB
cd %OHRRPGCE%
%QBDIR%\qb.exe /lallmodex /ah %1
cd %OHRRPGCE%
GOTO FINISH

:NOQB
ECHO QB.EXE QuickBasic 4.5 editor is not present in %QBDIR%
ECHO Please read README.TXT

:FINISH