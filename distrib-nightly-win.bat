REM *WARNING* Do not schedule this batch file to be automatically
REM run from the same copy of the sources that it updates. That
REM would be equivalent to allowing any developer with write access
REM to the repository full control of your computer. Instead schedule
REM this script to be run from a manually updated copy, and pay
REM attention to changes to it.

cd c:\nightly\ohrrpgce
svn cleanup
svn update

del game*.exe
del custom*.exe
call makegame-gfx-music fb native
call makeedit-gfx-music fb native
call nightly-gfx-music fb native

del game*.exe
del custom*.exe
call makegame-gfx-music fb native2
call makeedit-gfx-music fb native2
call nightly-gfx-music fb native2

del game*.exe
del custom*.exe
call makegame-gfx-music fb sdl
call makeedit-gfx-music fb sdl
call nightly-gfx-music fb sdl

del game*.exe
del custom*.exe
call makegame-gfx-music smooth2x native
call makeedit-gfx-music smooth2x native
call nightly-gfx-music smooth2x native

del game*.exe
del custom*.exe
call makegame-gfx-music smooth2x native2
call makeedit-gfx-music smooth2x native2
call nightly-gfx-music smooth2x native2

del game*.exe
del custom*.exe
call makegame-gfx-music smooth2x sdl
call makeedit-gfx-music smooth2x sdl
call nightly-gfx-music smooth2x sdl

del game*.exe
del custom*.exe
call makegame-gfx-music alleg native
call makeedit-gfx-music alleg native
call nightly-gfx-music alleg native

del game*.exe
del custom*.exe
call makegame-gfx-music alleg sdl
call makeedit-gfx-music alleg sdl
call nightly-gfx-music alleg sdl

del game*.exe
del custom*.exe
call makegame-gfx-music alleg allegro
call makeedit-gfx-music alleg allegro
call nightly-gfx-music alleg allegro

del game*.exe
del custom*.exe
call makegame-gfx-music sdl sdl
call makeedit-gfx-music sdl sdl
call nightly-gfx-music sdl sdl

del game*.exe
del custom*.exe
call makegame-gfx-music fb silence
call makeedit-gfx-music fb silence
call nightly-gfx-music fb silence

Echo upload plotdict.xml
pscp -i C:\progra~1\putty\id_rsa.ppk docs\plotdict.xml james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/docs/

call makeutil.bat
del distrib\ohrrpgce-util.zip
IF NOT EXIST unlump.exe GOTO NOUTIL
IF NOT EXIST relump.exe GOTO NOUTIL
support\zip distrib\ohrrpgce-util.zip unlump.exe relump.exe LICENSE-binary.txt
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
support\zip distrib\bam2mid.zip bam2mid.exe bam2mid.txt bam2mid.bas banks.bi LICENSE.txt make-bam2mid.bat make-bam2mid.sh
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\bam2mid.zip james_paige@motherhamster.org:HamsterRepublic.com/ohrrpgce/nightly/
:NOBAM2MID
