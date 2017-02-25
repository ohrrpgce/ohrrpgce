@ECHO OFF

CHOICE /m "NIGHTLY BUILD WILL START IN 10 SECONDS" /n /d "Y" /t 10

CD c:\nightly\ohrrpgce
CALL distrib-nightly-win.bat 1> c:\nightly\distrib-out.txt 2>&1
CD C:\nightly

REM https://github.com/muquit/mailsend
mailsend.exe -to cron@rpg.hamsterrepublic.com -from cron@rpg.hamsterrepublic.com -sub "OHRRPGCE Windows nightly build" -smtp sub5.mail.dreamhost.com -port 465 -user cron@rpg.hamsterrepublic.com -pass REPLACEWITHREALPASSWORD -ssl -auth -mime-type "text/plain" -msg-body distrib-out.txt

CHOICE /m "DONE WILL SHUT DOWN IN 5 SECONDS" /n /d "Y" /t 5
shutdown /p