@ECHO OFF
IF NOT EXIST %EUDIR%\bin\euc.exe GOTO NOEUPHORIA
euc -gcc hspeak.exw
GOTO DONE

:NOEUPHORIA
ECHO To compile HSPEAK you must install Euphoria. It is Free Open-Source software.
ECHO http://www.RapidEuphoria.com/

:DONE