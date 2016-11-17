:LOOP

CD c:\nightly\ohrrpgce
CALL distrib-nightly-win.bat 1> c:\nightly\distrib-out.txt 2>&1
CD C:\nightly

REM https://github.com/muquit/mailsend
mailsend.exe -to Bob@HamsterRepublic.com -from jamesp@westcoastaerospace.com -sub "OHRRPGCE Windows nightly build" -smtp postal.westcoastaerospace.com -mime-type "text/plain" -msg-body distrib-out.txt

ECHO Sleeping for 6 hours...
SLEEP 21600

goto LOOP 