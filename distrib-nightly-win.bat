REM *WARNING* Do not schedule this batch file to be automatically
REM run from the same copy of the sources that it updates. That
REM would be equivalent to allowing any developer with write access
REM to the repository full control of your computer. Instead schedule
REM this script to be run from a manually updated copy, and pay
REM attention to changes to it.

cd c:\nightly\ohrrpgce
svn update
call makegame
del distrib\ohrrpgce-binary-win-nightly.zip
support\pkzip distrib\ohrrpgce-binary-win-nightly.zip game.exe whatsnew.txt LICENSE-binary.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\ohrrpgce-binary-win-nightly.zip spam@brionne.cyberverse.com:web/html/ohrrpgce/archive/
