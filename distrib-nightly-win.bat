REM *WARNING* Scheduling this batch file to be automatically
REM run is equivalent to allowing any developer with write access
REM to the repository full control of your build computer. Thank
REM goodness James trusts the other devs ;)

cd c:\nightly\ohrrpgce
svn cleanup
svn update
svn info > svninfo.txt

CALL distrib.bat
CALL distver.bat
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\ohrrpgce-win-installer-%OHRVERDATE%-%OHRVERCODE%.exe james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/ohrrpgce-wip-win-installer.exe

del game*.exe
del custom*.exe
call make.bat fb sdl
call nightly-gfx-music fb sdl

del game*.exe
del custom*.exe
call make.bat fb native
call nightly-gfx-music fb native

del game*.exe
del custom*.exe
call make.bat fb native2
call nightly-gfx-music fb native2

del game*.exe
del custom*.exe
call make.bat alleg native
call nightly-gfx-music alleg native

del game*.exe
del custom*.exe
call make.bat alleg sdl
call nightly-gfx-music alleg sdl

del game*.exe
del custom*.exe
call make.bat alleg allegro
call nightly-gfx-music alleg allegro

del game*.exe
del custom*.exe
call make.bat sdl sdl
call nightly-gfx-music sdl sdl

del game*.exe
del custom*.exe
call make.bat fb silence
call nightly-gfx-music fb silence

del game*.exe
del custom*.exe
call make.bat fb sdl -g
call nightly-gfx-music fb sdl -debug

del game*.exe
del custom*.exe
call make.bat fb sdl -g -exx -s console
call nightly-gfx-music fb sdl -debug-exx

Echo upload plotdict.xml
pscp -i C:\progra~1\putty\id_rsa.ppk docs\plotdict.xml james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/docs/

call makeutil.bat
del distrib\ohrrpgce-util.zip
IF NOT EXIST unlump.exe GOTO NOUTIL
IF NOT EXIST relump.exe GOTO NOUTIL
support\zip distrib\ohrrpgce-util.zip unlump.exe relump.exe LICENSE-binary.txt svninfo.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\ohrrpgce-util.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
:NOUTIL

del distrib\hspeak-win-nightly.zip
call makehspeak.bat
IF NOT EXIST hspeak.exe GOTO NOHSPEAK
support\zip distrib\hspeak-win-nightly.zip hspeak.exe hspeak.exw hsspiffy.e LICENSE.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\hspeak-win-nightly.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
:NOHSPEAK

del distrib\bam2mid.zip
call make-bam2mid.bat
IF NOT EXIST bam2mid.exe GOTO NOBAM2MID
support\zip distrib\bam2mid.zip bam2mid.exe bam2mid.txt bam2mid.bas banks.bi LICENSE.txt make-bam2mid.bat make-bam2mid.sh svninfo.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\bam2mid.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
:NOBAM2MID

del distrib\madplay+oggenc.zip
support\zip distrib\madplay+oggenc.zip support\madplay.exe support\oggenc.exe support\LICENSE-*.txt LICENSE.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\madplay+oggenc.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/