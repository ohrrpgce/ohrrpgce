@ECHO OFF
IF NOT EXIST %EUDIR%\bin\exw.exe GOTO NOEUPHORIA
ECHO. | exwc %EUDIR%\source\bind.ex -w32 hspeak.exw
GOTO DONE

:NOEUPHORIA
ECHO To compile HSPEAK you must install Euphoria. It is Free Open-Source software.
ECHO http://www.RapidEuphoria.com/

:DONE