IF NOT EXIST %QBDIR%\%QBEDIT% GOTO NOQB
cd %OHRRPGCE%
%QBDIR%\%QBEDIT% /lallmodex /ah %1
cd %OHRRPGCE%
GOTO FINISH

:NOQB
ECHO %QBEDIT% QuickBasic %QBVER% editor is not present in %QBDIR%
ECHO Please read README.TXT

:FINISH