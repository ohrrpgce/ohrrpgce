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

Echo upload plotdict.xml
pscp -i C:\progra~1\putty\id_rsa.ppk docs\plotdict.xml spam@brionne.cyberverse.com:web/html/ohrrpgce/docs/

call makeutil.bat
del distrib\ohrrpgce-util.zip
support\zip distrib\ohrrpgce-util.zip unlump.exe relump.exe LICENSE-binary.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\ohrrpgce-util.zip spam@brionne.cyberverse.com:web/html/ohrrpgce/nightly/

del distrib\hspeak-win-nightly.zip
call bindw -clear_routines hspeak.exw
support\zip distrib\hspeak-win-nightly.zip hspeak.exe hspeak.exw hsspiffy.e LICENSE.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\hspeak-win-nightly.zip spam@brionne.cyberverse.com:web/html/ohrrpgce/nightly/

del distrib\bam2mid.zip
call make-bam2mid.bat
support\zip distrib\bam2mid.zip bam2mid.exe bam2mid.txt b2m.bas bam2mid.bas banks.bi LICENSE.txt
pscp -i C:\progra~1\putty\id_rsa.ppk distrib\bam2mid.zip spam@brionne.cyberverse.com:web/html/ohrrpgce/nightly/
