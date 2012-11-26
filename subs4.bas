'OHRRPGCE CUSTOM - Misc editors
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

#include "config.bi"
#include "allmodex.bi"
#include "gfx_newRenderPlan.bi"  'for Surface
#include "common.bi"
#include "bcommon.bi"
#include "customsubs.bi"
#include "cglobals.bi"

#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"
#include "loading.bi"

'FIXME: Not yet exposed in allmodex.bi
declare sub surface_export_bmp24 (f as string, byval surf as Surface Ptr)

'local subs and functions
DECLARE SUB generalscriptsmenu ()
DECLARE SUB generalmusicsfxmenu ()
DECLARE SUB masterpalettemenu ()
DECLARE SUB statcapsmenu ()
DECLARE SUB battleoptionsmenu ()
DECLARE SUB equipmergemenu ()
DECLARE FUNCTION importmasterpal (f as string, byval palnum as integer) as integer
DECLARE SUB titlescreenbrowse ()
DECLARE SUB import_convert_mp3(byref mp3 as string, byref oggtemp as string)
DECLARE SUB import_convert_wav(byref wav as string, byref oggtemp as string)
DECLARE SUB inputpasw ()
DECLARE FUNCTION dissolve_type_caption(n as integer) as string
DECLARE SUB nearestui (byval mimicpal as integer, newpal() as RGBcolor, newui() as integer)
DECLARE SUB remappalette (oldmaster() as RGBcolor, oldpal() as integer, newmaster() as RGBcolor, newpal() as integer)
DECLARE SUB importsong_save_song_data(sname as string, byval snum as integer)
DECLARE SUB importsong_exportsong(songfile as string, bamfile as string, file_ext as string)
DECLARE SUB importsong_get_song_info (sname as string, songfile as string, byval snum as integer, file_ext as string, menu() as string, byref optionsbottom as integer)
DECLARE SUB importsong_import_song_file (sname as string, songfile as string, byval snum as integer)
DECLARE SUB importsfx_get_sfx_info(sname as string, sfxfile as string, byval snum as integer, file_ext as string, menu() as string)
DECLARE SUB importsfx_save_sfx_data(sname as string, byval snum as integer)
DECLARE SUB importsfx_exportsfx(sfxfile as string, file_ext as string)
DECLARE SUB importsfx_importsfxfile(sname as string, sfxfile as string, byval snum as integer, file_ext as string)

SUB vehicles

DIM menu(20) as string
DIM veh(39) as integer
DIM min(39) as integer
DIM max(39) as integer
DIM offset(39) as integer
DIM vehbit(15) as string
DIM tiletype(8) as string
DIM vehname as string = ""

DIM pt as integer = 0
DIM csr as integer = 0
DIM top as integer = 0
DIM tog as integer

vehbit(0) = "Pass through walls"
vehbit(1) = "Pass through NPCs"
vehbit(2) = "Enable NPC activation"
vehbit(3) = "Enable door use"
vehbit(4) = "Do not hide leader"
vehbit(5) = "Do not hide party"
vehbit(6) = "Dismount one space ahead"
vehbit(7) = "Pass walls while dismounting"
vehbit(8) = "Disable flying shadow"

tiletype(0) = "default"
tiletype(1) = "A"
tiletype(2) = "B"
tiletype(3) = "A and B"
tiletype(4) = "A or B"
tiletype(5) = "not A"
tiletype(6) = "not B"
tiletype(7) = "neither A nor B"
tiletype(8) = "everywhere"

min(3) = 0: max(3) = 5: offset(3) = 8             'speed
FOR i as integer = 0 TO 3
 min(5 + i) = 0: max(5 + i) = 8: offset(5 + i) = 17 + i
NEXT i
min(9) = -1: max(9) = 255: offset(9) = 11 'battles
min(10) = -2: max(10) = gen(genMaxRegularScript): offset(10) = 12 'use button
min(11) = -2: max(11) = gen(genMaxRegularScript): offset(11) = 13 'menu button
min(12) = -max_tag(): max(12) = max_tag(): offset(12) = 14 'tag
min(13) = gen(genMaxRegularScript) * -1: max(13) = gen(genMaxTextbox): offset(13) = 15'mount
min(14) = gen(genMaxRegularScript) * -1: max(14) = gen(genMaxTextbox): offset(14) = 16'dismount
min(15) = 0: max(15) = 99: offset(15) = 21'dismount

LoadVehicle game + ".veh", veh(), vehname, pt
GOSUB vehmenu

setkeys YES
DO
 setwait 55
 setkeys YES
 tog = tog XOR 1
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "vehicle_editor"
 usemenu csr, top, 0, 15, 22
 SELECT CASE csr
  CASE 0
   IF enter_or_space() THEN
    EXIT DO
   END IF
  CASE 1
   DIM savept as integer = pt
   IF intgrabber_with_addset(pt, 0, gen(genMaxVehicle), 32767, "vehicle") THEN
    SaveVehicle game + ".veh", veh(), vehname, savept
    IF pt > gen(genMaxVehicle) THEN  '--adding set
     gen(genMaxVehicle) = pt
     flusharray veh()
     vehname = ""
    ELSE
     LoadVehicle game + ".veh", veh(), vehname, pt
    END IF
    GOSUB vehmenu
   END IF
  CASE 2
   DIM oldname as string = vehname
   strgrabber vehname, 15
   IF oldname <> vehname THEN GOSUB vehmenu
  CASE 3, 5 TO 9, 15
   IF intgrabber(veh(offset(csr)), min(csr), max(csr)) THEN
    GOSUB vehmenu
   END IF
  CASE 12 '--tags
   IF tag_grabber(veh(offset(csr)), , , NO) THEN
    GOSUB vehmenu
   END IF
  CASE 4
   IF enter_or_space() THEN
    editbitset veh(), 9, 8, vehbit(), "vehicle_bitsets"
   END IF
  CASE 10, 11
   IF enter_or_space() THEN
    veh(offset(csr)) = large(0, veh(offset(csr)))
    scriptbrowse veh(offset(csr)), plottrigger, "vehicle plotscript"
    GOSUB vehmenu
   ELSEIF scrintgrabber(veh(offset(csr)), min(csr), max(csr), scLeft, scRight, 1, plottrigger) THEN
    GOSUB vehmenu
   END IF
  CASE 13, 14
   IF enter_or_space() THEN
    DIM temptrig as integer = large(0, -veh(offset(csr)))
    scriptbrowse temptrig, plottrigger, "vehicle plotscript"
    veh(offset(csr)) = -temptrig
    GOSUB vehmenu
   ELSEIF scrintgrabber(veh(offset(csr)), min(csr), max(csr), scLeft, scRight, -1, plottrigger) THEN
    GOSUB vehmenu
   END IF
 END SELECT
 clearpage dpage
 standardmenu menu(), 15, 15, csr, top, 0, 0, dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
SaveVehicle game + ".veh", veh(), vehname, pt
EXIT SUB

vehmenu:
menu(0) = "Previous Menu"
menu(1) = "Vehicle " & pt
menu(2) = "Name: " + vehname

IF veh(offset(3)) = 3 THEN
 menu(3) = "Speed: 10"
ELSE
 menu(3) = "Speed: " & veh(8)
END IF

menu(4) = "Vehicle Bitsets..." '9,10

menu(5) = "Override walls: "
menu(6) = "Blocked by: "
menu(7) = "Mount from: "
menu(8) = "Dismount to: "
FOR i as integer = 0 TO 3
 menu(5 + i) = menu(5 + i) + tiletype(bound(veh(offset(5 + i)), 0, 8))
NEXT i

DIM tmp as string

SELECT CASE veh(offset(9))
 CASE -1
  tmp = "disabled"
 CASE 0
  tmp = "enabled"
 CASE ELSE
  tmp = "formation set " & veh(offset(9))
END SELECT
menu(9) = "Random Battles: " + tmp '11

FOR i as integer = 0 TO 1
 SELECT CASE veh(offset(10 + i))
  CASE -2
   tmp = "disabled"
  CASE -1
   tmp = "menu"
  CASE 0
   tmp = "dismount"
  CASE ELSE
   tmp = "script " + scriptname(ABS(veh(offset(10 + i))))
 END SELECT
 IF i = 0 THEN menu(10 + i) = "Use button: " + tmp'12
 IF i = 1 THEN menu(10 + i) = "Menu button: " + tmp'13
NEXT i

menu(12) = tag_set_caption(veh(offset(12)), "If riding set tag")

SELECT CASE veh(offset(13))
 CASE 0
  tmp = "[script/textbox]"
 CASE IS < 0
  tmp = "run script " + scriptname(ABS(veh(offset(13))))
 CASE IS > 0
  tmp = "text box " & veh(offset(13))
END SELECT
menu(13) = "On Mount: " + tmp

SELECT CASE veh(offset(14))
 CASE 0
  tmp = "[script/textbox]"
 CASE IS < 0
  tmp = "run script " + scriptname(ABS(veh(offset(14))))
 CASE IS > 0
  tmp = "text box " & veh(offset(14))
END SELECT
menu(14) = "On Dismount: " + tmp

menu(15) = "Elevation: " & veh(offset(15)) & " pixels"
RETRACE

END SUB

SUB generalscriptsmenu ()
DIM menu(3) as string
DIM scrname(3) as string
DIM scriptgenoff(3) as integer = {0, 41, 42, 57}
menu(0) = "Previous Menu"
menu(1) = "new-game plotscript"
menu(2) = "game-over plotscript"
menu(3) = "load-game plotscript"
scrname(0) = ""
FOR i as integer = 1 TO 3
 scrname(i) = ": " + scriptname(gen(scriptgenoff(i)))
NEXT

DIM pt as integer = 0
DIM menusize as integer = 3
DIM tog as integer
setkeys
DO
 tog = tog XOR 1
 setwait 55
 setkeys
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "global_scripts"
 usemenu pt, 0, 0, menusize, 24
 IF pt = 0 THEN
  IF enter_or_space() THEN EXIT DO
 ELSE
  IF enter_or_space() THEN
   scrname(pt) = ": " & scriptbrowse_string(gen(scriptgenoff(pt)), plottrigger, menu(pt))
  ELSEIF scrintgrabber(gen(scriptgenoff(pt)), 0, 0, scLeft, scRight, 1, plottrigger) THEN
   scrname(pt) = ": " + scriptname(gen(scriptgenoff(pt)))
  END IF
 END IF
 clearpage dpage
 FOR i as integer = 0 TO menusize
  IF pt = i THEN textcolor uilook(uiSelectedItem + tog), 0 ELSE textcolor uilook(uiMenuItem), 0
  printstr menu(i) + scrname(i), 0, i * 8, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
END SUB

SUB generalmusicsfxmenu ()
  CONST num as integer = 15
  CONST lastmusicitem as integer = 3
  DIM as string menu(num), disp(num)
  DIM as integer index(1 to num) = {genTitleMus, genBatMus, genVictMus, genAcceptSFX, genCancelSFX, genCursorSFX, genTextboxLine, genDefaultDeathSFX, genItemLearnSFX, genCantLearnSFX, genBuySFX, genHireSFX, genSellSFX, genCantBuySFX, genCantSellSFX}
  DIM as integer menutop

  disp(0) = "Previous Menu" 'don't need menu(0)
  menu(1) = "Title Music: "
  menu(2) = "Default Battle Music: "
  menu(3) = "Battle Victory Music: "
  menu(4) = "Accept Sound: "
  menu(5) = "Cancel Sound: "
  menu(6) = "Cursor Sound: "
  menu(7) = "Textbox Line Sound: "
  menu(8) = "Default Enemy Death: "
  menu(9) = "Learnt From Item Sound: "
  menu(10) = "Can't Learn From Item Sound: "
  menu(11) = "Buy Item Sound: "
  menu(12) = "Hire Hero Sound: "
  menu(13) = "Sell Item Sound: "
  menu(14) = "Can't Buy Sound: "
  menu(15) = "Can't Sell Sound: "

  FOR i as integer = 1 to num
    IF gen(index(i)) > 0 THEN
      IF i <= lastmusicitem THEN
        disp(i) = menu(i) & getsongname(gen(index(i)) - 1, -1)  'prefixes number
      ELSE
        disp(i) = menu(i) & (gen(index(i)) - 1) & " " & getsfxname(gen(index(i)) - 1)
      END IF
    ELSE
      disp(i) = menu(i) & "None"
    END IF
  NEXT
  DIM pt as integer = 0
  DIM menusize as integer = num
  setkeys
  DO
    setwait 55
    setkeys

    IF keyval(scESC) > 1 THEN EXIT DO
    IF keyval(scF1) > 1 THEN show_help "general_music_sfx"
    usemenu pt, 0, 0, menusize, 24

    IF enter_or_space() THEN
      SELECT CASE as CONST pt
      CASE 0
        EXIT DO
      CASE 1 TO lastmusicitem
        IF gen(index(pt)) > 0 THEN playsongnum gen(index(pt)) - 1
      CASE lastmusicitem + 1 TO num
        IF gen(index(pt)) > 0 THEN playsfx gen(index(pt)) - 1
      END SELECT
    END IF

    SELECT CASE as CONST pt
    CASE 1 TO lastmusicitem
      IF zintgrabber(gen(index(pt)), -1, gen(genMaxSong)) THEN
        music_stop
        IF gen(index(pt)) > 0 THEN
          disp(pt) = menu(pt) & getsongname(gen(index(pt)) - 1, -1)  'prefixes number
        ELSE
          disp(pt) = menu(pt) & "None"
        END IF
      END IF
    CASE lastmusicitem + 1 TO num
      IF zintgrabber(gen(index(pt)), -1, gen(genMaxSFX)) THEN
        resetsfx
        IF gen(index(pt)) > 0 THEN
          disp(pt) = menu(pt) & (gen(index(pt))-1) & " " & getsfxname(gen(index(pt)) - 1)
        ELSE
          disp(pt) = menu(pt) & "None"
        END IF
      END IF
    END SELECT

    clearpage dpage
    standardmenu disp(), num, 22, pt, menutop, 0, 0, dpage

    SWAP vpage, dpage
    setvispage vpage
    dowait
  LOOP
  music_stop
  resetsfx
END SUB

SUB delete_song (byval songnum as integer, songfile as string)
 #IFDEF __FB_WIN32__
  'Only needed on windows, and not currently implemented on unix anyway
  IF slave_channel <> NULL_CHANNEL THEN
   'Close Music message
   DIM msg as string = "CM " & songnum
   IF channel_write_line(slave_channel, msg) THEN
    channel_wait_for_msg(slave_channel, "CM ", "", 1500)
   END IF
  END IF
 #ENDIF
 safekill songfile
 'FIXME: handle deleting from rpgdirs (bug 247)... and the same for soundeffects
END SUB

SUB importsong ()
DIM oggtemp as string
DIM menu(10) as string
menu(0) = "Previous Menu"
menu(3) = "Import Song..."
menu(4) = "Export Song..."
menu(5) = "Delete Song"

DIM csr as integer = 1
DIM snum as integer = 0
DIM sname as string = ""
DIM songfile as string = ""
DIM bamfile as string = ""
DIM optionsbottom as integer = 0
DIM newsong as integer
DIM file_ext as string
importsong_get_song_info sname, songfile, snum, file_ext, menu(), optionsbottom

setkeys YES
DO
 setwait 55
 setkeys YES
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "import_songs"

 usemenu csr, 0, 0, optionsbottom, 22

 IF csr = 2 AND songfile <> "" THEN
  strgrabber sname, 30
  menu(2) = "Name: " + sname
 ELSE
  '-- check for switching song
  newsong = snum
  IF intgrabber(newsong, 0, gen(genMaxSong), scLeftCaret, scRightCaret) THEN
   importsong_save_song_data sname, snum
   snum = newsong
   importsong_get_song_info sname, songfile, snum, file_ext, menu(), optionsbottom
  END IF
  IF keyval(scLeft) > 1 AND snum > 0 THEN
   importsong_save_song_data sname, snum
   snum = snum - 1
   importsong_get_song_info sname, songfile, snum, file_ext, menu(), optionsbottom
  END IF
  IF keyval(scRight) > 1 AND snum < 32767 THEN
   importsong_save_song_data sname, snum
   snum = snum + 1
   IF needaddset(snum, gen(genMaxSong), "song") THEN sname = ""
   importsong_get_song_info sname, songfile, snum, file_ext, menu(), optionsbottom
  END IF
 END IF
 IF enter_or_space() THEN
  IF csr = 0 THEN EXIT DO
  IF csr = 3 THEN
   importsong_import_song_file sname, songfile, snum
   importsong_get_song_info sname, songfile, snum, file_ext, menu(), optionsbottom
  END IF
  IF csr = 4 AND songfile <> "" THEN importsong_exportsong songfile, bamfile, file_ext
  IF csr = 5 AND songfile <> "" THEN  'delete song
   IF yesno("Really delete this song?", NO, NO) THEN
    music_stop
    'closemusic  'music_stop not always enough to cause the music backend to let go of the damn file!
    'setupmusic
    delete_song snum, songfile
    safekill bamfile
    IF slave_channel <> NULL_CHANNEL THEN send_lump_modified_msg(songfile)  'only need to send any valid filename for this song
    importsong_get_song_info sname, songfile, snum, file_ext, menu(), optionsbottom
   END IF
  END IF
  IF csr = 6 THEN  'delete BAM fallback
   IF yesno("Really delete this BAM song?", NO, NO) THEN
    safekill bamfile
    importsong_get_song_info sname, songfile, snum, file_ext, menu(), optionsbottom
    csr = 0
   END IF
  END IF
 END IF

 clearpage dpage
 standardmenu menu(), 10, 22, csr, 0, 0, 0, dpage

 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
importsong_save_song_data sname, snum
music_stop
EXIT SUB

END SUB

SUB importsong_import_song_file (sname as string, songfile as string, byval snum as integer)
 STATIC default as string
 music_stop
 'closemusic  'music_stop not always enough to cause the music backend to let go of the damn file!
 'setupmusic

 'browse for new song
 DIM sourcesong as string = browse(5, default, "", "",, "browse_import_song")

 'Get song name
 DIM a as string = trimextension(trimpath(sourcesong))

 'Convert MP3
 DIM oggtemp as string
 IF getmusictype(sourcesong) = FORMAT_MP3 THEN
  import_convert_mp3 sourcesong, oggtemp
 ELSE
  oggtemp = ""
 END IF

 'If no song was selected, go back
 IF sourcesong = "" THEN
  EXIT SUB
 END IF

 delete_song snum, songfile

 sname = a

 'generate lump name
 DIM extension as string = LCASE(justextension(sourcesong))
 IF extension = "bam" AND snum <= 99 THEN
  songfile = game + "." & snum
 ELSE
  songfile = workingdir & SLASH & "song" & snum & "." & extension
 END IF

 'Copy in new lump (this implicitly sends a notification to Game if it's been spawned)
 copyfile sourcesong, songfile

 IF oggtemp <> "" THEN KILL oggtemp

 importsong_save_song_data sname, snum
END SUB

SUB importsong_get_song_info (sname as string, songfile as string, byval snum as integer, file_ext as string, menu() as string, byref optionsbottom as integer)
 music_stop

 DIM temp as string
 '-- first job: find the song's name
 temp = workingdir & SLASH & "song" & snum
 songfile = ""
 DIM songtype as string = "NO FILE"
 '-- BAM special case and least desirable, so check first and override
 IF snum > 99 THEN
  IF isfile(temp & ".bam") THEN
   file_ext = ".bam"
   songfile = temp & file_ext
   songtype = "Bob's Adlib Music (BAM)"
  END IF
 ELSE
  IF isfile(game & "." & snum) THEN
   file_ext = ".bam"
   songfile = game & "." & snum
   songtype = "Bob's Adlib Music (BAM)"
  END IF
 END IF
 DIM bamfile as string = songfile

 IF isfile(temp & ".ogg") THEN
  file_ext = ".ogg"
  songfile = temp & file_ext
  songtype = "OGG Vorbis (OGG)"
 ELSEIF isfile(temp & ".s3m") THEN
  file_ext = ".s3m"
  songfile = temp & file_ext
  songtype = "Screamtracker (S3M)"
 ELSEIF isfile(temp & ".it") THEN
  file_ext = ".it"
  songfile = temp & file_ext
  songtype = "Impulse Tracker (IT)"
 ELSEIF isfile(temp & ".xm") THEN
  file_ext = ".xm"
  songfile = temp & file_ext
  songtype = "Extended Module (XM)"
 ELSEIF isfile(temp & ".mod") THEN
  file_ext = ".mod"
  songfile = temp & file_ext
  songtype = "Module (MOD)"
 ELSEIF isfile(temp & ".mp3") THEN ' Obsolete. only present in some Ubersetzung WIP games
  file_ext = ".mp3"
  songfile = temp & file_ext
  songtype = "MPEG Layer III (MP3) OBSOLETE"
 ELSEIF isfile(temp & ".mid") THEN
  file_ext = ".mid"
  songfile = temp & file_ext
  songtype = "MIDI Music (MID)"
 END IF
 '--add more formats here

 sname = getsongname(snum)

 IF songfile <> "" THEN '--song exists
  loadsong songfile
 ELSE
  sname = ""
 END IF

 menu(1) = "<- Song " & snum & " of " & gen(genMaxSong) & " ->"
 IF songfile <> "" THEN menu(2) = "Name: " & sname ELSE menu(2) = "-Unused-"
 menu(7) = ""
 menu(8) = "Type: " & songtype
 menu(9) = "Filesize: " & filesize(songfile)
 IF bamfile <> songfile AND bamfile <> "" THEN
  menu(10) = "BAM fallback exists. Filesize: " & filesize(bamfile)
  menu(6) = "Delete BAM fallback"
  optionsbottom = 6
 ELSE
  menu(10) = ""
  menu(6) = ""
  optionsbottom = 5
 END IF
 '-- add author, length, etc, info here
END SUB

SUB importsong_exportsong(songfile as string, bamfile as string, file_ext as string)
'exportsong:
 IF bamfile <> songfile AND bamfile <> "" THEN
  DIM submenu(2) as string
  submenu(0) = "Export " + file_ext + " file"
  submenu(1) = "Export .bam fallback file"
  submenu(2) = "Cancel"
  DIM choice as integer = sublist(submenu(), "export_song")
  IF choice = 1 THEN file_ext = ".bam" : songfile = bamfile
  IF choice = 2 THEN EXIT SUB
 END IF
 DIM query as string = "Name of file to export to?"
 DIM outfile as string = inputfilename(query, file_ext, "", "input_file_export_song")
 IF outfile = "" THEN EXIT SUB
 copyfile songfile, outfile & file_ext
END SUB

SUB importsong_save_song_data(sname as string, byval snum as integer)
 DIM songbuf(dimbinsize(binSONGDATA)) as integer
 setpicstuf songbuf(), curbinsize(binSONGDATA), -1
 writebinstring sname, songbuf(), 0, 30
 storeset workingdir & SLASH & "songdata.bin", snum, 0
END SUB

SUB importsfx ()

DIM menu(11) as string
DIM submenu(2) as string
DIM optionsbottom as integer = 6
menu(0) = "Previous Menu"
menu(3) = "Import Sound..."
menu(4) = "Export Sound..."
menu(5) = "Delete Sound"
menu(6) = "Play Sound"

DIM csr as integer = 1
DIM snum as integer = 0
DIM sname as string = ""
DIM sfxfile as string = ""
DIM newsfx as integer
DIM file_ext as string
importsfx_get_sfx_info sname, sfxfile, snum, file_ext, menu()

setkeys YES
DO
 setwait 55
 setkeys YES
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "import_sfx"

 usemenu csr, 0, 0, optionsbottom, 22

 IF csr = 2 AND sfxfile <> "" THEN
  strgrabber sname, 30
  menu(2) = "Name: " + sname
 ELSE
  '-- check for switching sfx
  newsfx = snum
  IF intgrabber(newsfx, 0, gen(genMaxSFX), scLeftCaret, scRightCaret) THEN
   importsfx_save_sfx_data sname, snum
   snum = newsfx
   importsfx_get_sfx_info sname, sfxfile, snum, file_ext, menu()
  END IF
  IF keyval(scLeft) > 1 AND snum > 0 THEN
   importsfx_save_sfx_data sname, snum
   snum = snum - 1
   importsfx_get_sfx_info sname, sfxfile, snum, file_ext, menu()
  END IF
  IF keyval(scRight) > 1 AND snum < 32767 THEN
   importsfx_save_sfx_data sname, snum
   snum = snum + 1
   IF needaddset(snum, gen(genMaxSFX), "sfx") THEN sname = ""
   importsfx_get_sfx_info sname, sfxfile, snum, file_ext, menu()
  END IF
 END IF
 IF enter_or_space() THEN
  SELECT CASE csr
  CASE 0
    EXIT DO
  CASE 3
    importsfx_importsfxfile sname, sfxfile, snum, file_ext
    importsfx_get_sfx_info sname, sfxfile, snum, file_ext, menu()
  CASE 4
    IF sfxfile <> "" THEN importsfx_exportsfx sfxfile, file_ext
  CASE 5
    IF sfxfile <> "" THEN  'delete sfx
      IF yesno("Really delete this sound?", NO, NO) THEN
        freesfx snum
        safekill sfxfile
        importsfx_get_sfx_info sname, sfxfile, snum, file_ext, menu()
      END IF
    END IF
  CASE 1, 6
    IF sfxfile <> "" THEN 'play sfx
      playsfx snum, 0
    END IF

  END SELECT
 END IF

 clearpage dpage
 standardmenu menu(), 10, 22, csr, 0, 0, 0, dpage

 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
importsfx_save_sfx_data sname, snum

EXIT SUB



END SUB

SUB importsfx_importsfxfile(sname as string, sfxfile as string, byval snum as integer, file_ext as string)
 STATIC default as string

 DIM sourcesfx as string = browse(6, default, "", "",, "browse_import_sfx")

 '-- get name
 DIM a as string = trimextension(trimpath(sourcesfx))

 'Convert MP3
 DIM oggtemp as string
 IF getmusictype(sourcesfx) = FORMAT_MP3 THEN
  import_convert_mp3 sourcesfx, oggtemp
 ELSEIF getmusictype(sourcesfx) = FORMAT_WAV THEN
  import_convert_wav sourcesfx, oggtemp
 ELSE
  oggtemp = ""
 END IF

 IF sourcesfx = "" THEN EXIT SUB

 safekill sfxfile

 sname = a

 '-- calculate lump name
 sfxfile = workingdir & SLASH & "sfx" & snum & "." & LCASE(justextension(sourcesfx))

 '--copy in the new lump
 copyfile sourcesfx, sfxfile

 IF oggtemp <> "" THEN KILL oggtemp

 '--save and update
 importsfx_save_sfx_data sname, snum
END SUB

SUB importsfx_exportsfx(sfxfile as string, file_ext as string)
 DIM query as string = "Name of file to export to?"
 DIM outfile as string = inputfilename(query, file_ext, "", "input_file_export_sfx")
 IF outfile = "" THEN EXIT SUB
 copyfile sfxfile, outfile & file_ext
END SUB

SUB importsfx_save_sfx_data(sname as string, byval snum as integer)
 freesfx snum
 DIM sfxbuf(dimbinsize(binSFXDATA)) as integer
 setpicstuf sfxbuf(), curbinsize(binSFXDATA), -1
 writebinstring sname, sfxbuf(), 0, 30
 storeset workingdir & SLASH & "sfxdata.bin", snum, 0
END SUB

SUB importsfx_get_sfx_info(sname as string, sfxfile as string, byval snum as integer, file_ext as string, menu() as string)
 '-- first job: find the sfx's name
 DIM temp as string = workingdir & SLASH & "sfx" & snum
 DIM sfxtype as string = "NO FILE"

 IF isfile(temp & ".ogg") THEN
  file_ext = ".ogg"
  sfxfile = temp & file_ext
  sfxtype = "OGG Vorbis (OGG)"
 ELSEIF isfile(temp & ".wav") THEN ' Obsolete, only present in Pre-Ubersetzung games
  file_ext = ".wav"
  sfxfile = temp & file_ext
  sfxtype = "Waveform (WAV) OBSOLETE"
 ELSEIF isfile(temp & ".mp3") THEN ' Obsolete, only present in some Ubersetzung WIP games
  file_ext = ".mp3"
  sfxfile = temp & file_ext
  sfxtype = "MPEG Layer III (MP3) OBSOLETE"
 END IF

 '--add more formats here

 if sfxfile <> "" then
  'playsfx snum, 0
  sname = getsfxname(snum)
 ELSE '--sfx doesn't exist
  sname = ""
 END IF

 menu(1) = "<- SFX " & snum & " of " & gen(genMaxSFX) & " ->"
 IF sfxfile <> "" THEN menu(2) = "Name: " & sname ELSE menu(2) = "-Unused-"
 menu(8) = ""
 menu(9) = "Type: " & sfxtype
 menu(10) = "Filesize: " & filesize(sfxfile)

 '-- add author, length, etc, info here
END SUB

SUB export_master_palette ()
 DIM filename as string

 filename = inputfilename("Name of 24 bit BMP file to export to?", ".bmp", "", "input_file_export_masterpal")
 IF filename = "" THEN EXIT SUB
 filename &= ".bmp"

 'FIXME: use this once rasterizer is always linked and Staging surfaces added
 'DIM outsurf as Surface ptr
 'gfx_surfaceCreate(16, 16, SF_32bit, SU_Staging, @outsurf)

 DIM outsurf as Surface
 outsurf.width = 16
 outsurf.height = 16
 outsurf.format = SF_32bit
 outsurf.pColorData = ALLOCATE(16 * 16 * 4)
 FOR i as integer = 0 TO 255
  outsurf.pColorData[i] = master(i).col
 NEXT
 surface_export_bmp24(filename, @outsurf)
 'gfx_surfaceDestroy(outsurf)
 DEALLOCATE(outsurf.pColorData)
END SUB

SUB masterpalettemenu
DIM menu(8) as string
DIM oldpal as integer
DIM csr as integer = 1
DIM tog as integer
DIM palnum as integer = activepalette
DIM col as integer
loadpalette master(), palnum
setpal master()
LoadUIColors uilook(), palnum
GOSUB buildmenu

setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1

 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "master_palette_menu"
 usemenu csr, 0, 0, UBOUND(menu), 10

 oldpal = palnum
 IF keyval(scRight) > 1 AND palnum = gen(genMaxMasterPal) THEN
  palnum += 1
  IF needaddset(palnum, gen(genMaxMasterPal), "Master Palette") THEN
   IF importmasterpal("", palnum) THEN
    setpal master()
    LoadUIColors uilook(), palnum
    GOSUB buildmenu     
   ELSE
    palnum -= 1
    gen(genMaxMasterPal) = palnum
   END IF
  END IF
  setkeys
 END IF
 IF csr = 1 THEN
  intgrabber(palnum, 0, gen(genMaxMasterPal))
 ELSE
  IF keyval(scLeft) > 1 THEN palnum += gen(genMaxMasterPal)
  IF keyval(scRight) > 1 THEN palnum += 1
  palnum = palnum MOD (gen(genMaxMasterPal) + 1)
 END IF
 IF palnum <> oldpal THEN
  loadpalette master(), palnum
  setpal master()
  LoadUIColors uilook(), palnum
  GOSUB buildmenu
 END IF

 IF enter_or_space() THEN
  SELECT CASE csr
  CASE 0
    EXIT DO
  CASE 2
    IF importmasterpal("", palnum) THEN
     setpal master()
     LoadUIColors uilook(), palnum
     GOSUB buildmenu
    END IF
  CASE 3
    export_master_palette
  CASE 4
    ui_color_editor palnum
  CASE 5
    nearestui activepalette, master(), uilook()
    SaveUIColors uilook(), palnum
  CASE 6
    LoadUIColors uilook(), activepalette
    SaveUIColors uilook(), palnum
  CASE 7
    gen(genMasterPal) = palnum
    'Instant live-previewing
    xbsave game + ".gen", gen(), 1000
    GOSUB buildmenu
  CASE 8
    activepalette = palnum
    GOSUB buildmenu
  END SELECT
 END IF

 'draw the menu
 clearpage dpage
 FOR i as integer = 0 TO UBOUND(menu)
  IF (i = 7 AND palnum = gen(genMasterPal)) OR ((i = 5 OR i = 6 OR i = 8) AND palnum = activepalette) THEN
   col = uilook(uiDisabledItem)
   IF csr = i THEN col = uilook(uiSelectedDisabled + tog)
  ELSE
   col = uilook(uiMenuItem)
   IF csr = i THEN col = uilook(uiSelectedItem + tog)
  END IF
  textcolor col, 0
  printstr menu(i), 0, i * 8, dpage
 NEXT i

 FOR i as integer = 0 TO 255
  rectangle 34 + (i MOD 16) * 16, 78 + (i \ 16) * 7, 12, 5, i, dpage
 NEXT
 IF csr = 4 OR csr = 5 OR csr = 6 THEN
  FOR i as integer = 0 TO uiColors
   drawbox 33 + (uilook(i) MOD 16) * 16, 77 + (uilook(i) \ 16) * 7, 14, 7, uilook(uiHighlight + tog), 1, dpage
  NEXT
 END IF

 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

IF activepalette <> palnum THEN
 loadpalette master(), activepalette
 setpal master()
 LoadUIColors uilook(), activepalette
END IF

EXIT SUB

buildmenu:
menu(0) = "Previous Menu"
menu(1) = "<- Master Palette " & palnum & " ->"
menu(2) = "Replace this Master Palette"
menu(3) = "Export this palette"
menu(4) = "Edit User Interface Colors..."
menu(5) = "Nearest-match active palette's UI colors"
menu(6) = "Copy active palette's UI data"
IF palnum = gen(genMasterPal) THEN
 menu(7) = "Current default in-game Master Palette"
ELSE
 menu(7) = "Set as in-game Master Palette"
END IF
IF palnum = activepalette THEN
 menu(8) = "Current active editing palette"
ELSE
 menu(8) = "Set as active editing palette"
END IF
RETRACE

END SUB

FUNCTION importmasterpal (f as string, byval palnum as integer) as integer
STATIC default as string
DIM bmpd as BitmapInfoHeader
IF f = "" THEN f = browse(4, default, "", "",, "browse_import_master_palette")
IF f <> "" THEN
 IF LCASE(justextension(f)) = "mas" THEN
  xbload f, buffer(), "MAS load error"
  convertpalette buffer(), master()
 ELSE
  bmpinfo(f, bmpd)
  IF bmpd.biBitCount = 24 THEN
   bitmap2pal f, master()
  ELSE
   loadbmppal f, master()
  END IF
 END IF
 'get a default set of ui colours - nearest match to the current
 nearestui activepalette, master(), uilook()

 IF palnum > gen(genMaxMasterPal) THEN gen(genMaxMasterPal) = palnum
 savepalette master(), palnum
 SaveUIColors uilook(), palnum
 RETURN -1
END IF
RETURN 0
END FUNCTION

SUB nearestui (byval mimicpal as integer, newmaster() as RGBcolor, newui() as integer)
 'finds the nearest match newui() in newpal() to mimicpal's ui colours
 DIM referencepal(255) as RGBcolor
 DIM referenceui(uiColors) as integer
 loadpalette referencepal(), mimicpal
 LoadUIColors referenceui(), mimicpal
 remappalette referencepal(), referenceui(), newmaster(), newui()
END SUB

SUB remappalette (oldmaster() as RGBcolor, oldpal() as integer, newmaster() as RGBcolor, newpal() as integer)
 FOR i as integer = 0 TO UBOUND(oldpal)
  WITH oldmaster(oldpal(i))
   IF .col = newmaster(oldpal(i)).col THEN
    newpal(i) = oldpal(i)
   ELSE
    newpal(i) = nearcolor(newmaster(), .r, .g, .b)
   END IF
  END WITH
 NEXT
END SUB

'FIXME:recursively enter backdrop editor instead?
SUB titlescreenbrowse
loadmxs game & ".mxs", gen(genTitle), vpages(2)
setkeys
DIM gcsr as integer = 0
DIM tog as integer
DIM col as integer
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "title_screen_browse"
 IF keyval(scUp) > 1 AND gcsr = 1 THEN gcsr = 0
 IF keyval(scDown) > 1 AND gcsr = 0 THEN gcsr = 1
 IF gcsr = 1 THEN
  IF intgrabber(gen(genTitle), 0, gen(genNumBackdrops) - 1) THEN 
   loadmxs game + ".mxs", gen(genTitle), vpages(2)
  END IF
 END IF
 IF enter_or_space() THEN
  IF gcsr = 0 THEN EXIT DO
 END IF

 copypage 2, dpage
 IF gcsr = 0 THEN col = uilook(uiSelectedItem + tog) ELSE col = uilook(uiMenuItem)
 edgeprint "Go Back", 1, 1, col, dpage
 IF gcsr = 1 THEN col = uilook(uiSelectedItem + tog) ELSE col = uilook(uiMenuItem)
 edgeprint CHR(27) + "Browse" + CHR(26), 1, 11, col, dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
END SUB

SUB import_convert_mp3(byref mp3 as string, byref oggtemp as string)
 DIM ogg_quality as integer
 IF (pick_ogg_quality(ogg_quality)) THEN mp3 = "" : EXIT SUB
 oggtemp = tmpdir & "temp." & randint(100000) & ".ogg"
 clearpage vpage
 centerbox 160, 100, 300, 20, 4, vpage
 edgeprint "Please wait, converting to OGG...", 28, 96, uilook(uiText), vpage
 setvispage vpage
 DIM ret as string = mp3_to_ogg(mp3, oggtemp, ogg_quality)
 IF LEN(ret) THEN
  visible_debug ret
  mp3 = ""
  EXIT SUB
 END IF
 IF NOT isfile(oggtemp) THEN
  'This branch ought to be unreachable...
  visible_debug "MP3 conversion failed."
  mp3 = ""
  EXIT SUB
 END IF
 mp3 = oggtemp
END SUB

SUB import_convert_wav(byref wav as string, byref oggtemp as string)
 DIM ogg_quality as integer
 IF (pick_ogg_quality(ogg_quality)) THEN wav = "" : EXIT SUB
 oggtemp = tmpdir & "temp." & randint(100000) & ".ogg"
 clearpage vpage
 centerbox 160, 100, 300, 20, 4, vpage
 edgeprint "Please wait, converting to OGG...", 28, 96, uilook(uiText), vpage
 setvispage vpage
 DIM ret as string = wav_to_ogg(wav, oggtemp, ogg_quality)
 IF LEN(ret) THEN
  visible_debug ret
  wav = ""
  EXIT SUB
 END IF
 IF NOT isfile(oggtemp) THEN
  'This branch ought to be unreachable...
  visible_debug "WAV conversion failed."
  wav = ""
  EXIT SUB
 END IF
 wav = oggtemp
END SUB

SUB inputpasw()
DIM tog as integer = 0
DIM oldpassword as integer = (checkpassword("") = 0)
DIM pas as string
setkeys YES
DO
 setwait 55
 setkeys YES
 tog = tog XOR 1
 IF keyval(scEsc) > 1 THEN EXIT DO
 IF keyval(scEnter) > 1 THEN
'  IF oldpassword = NO THEN
 writepassword pas
  EXIT DO
 END IF
 IF keyval(scF1) > 1 THEN show_help "input_password"
 strgrabber pas, 17
 clearpage dpage
 textcolor uilook(uiMenuItem), 0
 printstr "You can require a password for this", 0, 0, dpage
 printstr "game to be opened in " + CUSTOMEXE, 0, 8, dpage
 printstr "This does not encrypt your file, and", 0, 16, dpage
 printstr "should not be considered as any security", 0, 24, dpage
 IF oldpassword THEN
  printstr "PASSWORD SET. NEW PASSWORD:", 30, 64, dpage
  IF LEN(pas) = 0 THEN printstr "(Hit Enter to remove)", 30, 94, dpage
 ELSE
  printstr "NO PASSWORD. NEW PASSWORD:", 30, 64, dpage
 END IF
 IF LEN(pas) THEN
  textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight)
  printstr pas, 30, 74, dpage
 ELSE
  textcolor uilook(uiMenuItem), uilook(uiHighlight)
  printstr "(NONE)", 30, 74, dpage
 END IF
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
END SUB

FUNCTION dissolve_type_caption(n as integer) as string
 SELECT CASE n
  CASE 0: RETURN "Random scatter"
  CASE 1: RETURN "Crossfade"
  CASE 2: RETURN "Diagonal Vanish"
  CASE 3: RETURN "Sink into Ground"
  CASE 4: RETURN "Squash"
  CASE 5: RETURN "Melt"
  CASE 6: RETURN "Vapourise"
  CASE 7: RETURN "Phase out"
  CASE 8: RETURN "Squeeze"
  CASE 9: RETURN "Shrink"
  CASE 10: RETURN "Flicker"
  CASE ELSE: RETURN n & " Invalid!"
 END SELECT
END FUNCTION

SUB generate_battlesystem_menu(menu() as string)
 menu(0) = "Previous Menu"
 menu(1) = "Stat Caps..."
 menu(2) = "Hero Elemental Resistance Calculation..."

 menu(4) = "Number of Elements: " & gen(genNumElements)
 menu(5) = "Poison Indicator: " & gen(genPoison) & " " & CHR(gen(genPoison))
 menu(6) = "Stun Indicator: " & gen(genStun) & " " & CHR(gen(genStun))
 menu(7) = "Mute Indicator: " & gen(genMute) & " " & CHR(gen(genMute))
 menu(8) = "Default Enemy Dissolve: " & dissolve_type_caption(gen(genEnemyDissolve))
 menu(9) = "Damage Display Time: " & gen(genDamageDisplayTicks) & " ticks (" & seconds_estimate(gen(genDamageDisplayTicks)) & " sec)"
 menu(10) = "Damage Display Rises: " & gen(genDamageDisplayRise) & " pixels"
 menu(11) = "Experience given to heroes..."
 menu(12) = " ...swapped-out and unlocked: " & gen(genUnlockedReserveXP) & "%"
 menu(13) = " ...swapped-out and locked: " & gen(genLockedReserveXP) & "%"
 menu(14) = "Hero Weak state below: " & gen(genHeroWeakHP) & "% " & statnames(statHP)
 menu(15) = "Enemy Weak state below: " & gen(genEnemyWeakHP) & "% " & statnames(statHP)
 menu(17) = "View Experience Chart..."
 '--Disabled because it is not ready yet
 'menu(18) = "Stat Growth Options..."
END SUB

SUB battleoptionsmenu ()
 CONST maxMenu = 17
 DIM menu(maxMenu) as string
 DIM min(maxMenu) as integer
 DIM max(maxMenu) as integer
 DIM index(maxMenu) as integer
 DIM enabled(maxMenu) as integer
 DIM state as MenuState
 WITH state
  .size = 24
  .last = maxMenu
  .need_update = YES
 END WITH

 'I think these things are here (and not upgrade) because we don't want to force them on games
 IF gen(genPoison) <= 0 THEN gen(genPoison) = 161
 IF gen(genStun) <= 0 THEN gen(genStun) = 159
 IF gen(genMute) <= 0 THEN gen(genMute) = 163
 
 flusharray enabled(), UBOUND(enabled), YES
 enabled(3) = NO
 enabled(11) = NO
 enabled(16) = NO
 index(4) = genNumElements
 min(4) = 1
 max(4) = 64
 index(5) = genPoison
 index(6) = genStun
 index(7) = genMute
 FOR i as integer = 5 TO 7
  min(i) = 32
  max(i) = 255
 NEXT
 index(8) = genEnemyDissolve
 max(8) = dissolveTypeMax
 index(9) = genDamageDisplayTicks
 max(9) = 1000
 index(10) = genDamageDisplayRise
 max(10) = 1000
 min(10) = -1000
 index(12) = genUnlockedReserveXP
 max(12) = 1000
 index(13) = genLockedReserveXP
 max(13) = 1000
 min(14) = 1
 max(14) = 100
 index(14) = genHeroWeakHP
 min(15) = 1
 max(15) = 100
 index(15) = genEnemyWeakHP

 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "battle_system_options"
  usemenu state, enabled()
  IF enter_or_space() THEN
   IF state.pt = 0 THEN EXIT DO
   IF state.pt = 1 THEN statcapsmenu
   IF state.pt = 2 THEN equipmergemenu
   IF state.pt = 17 THEN experience_chart
   'IF state.pt = 18 THEN stat_growth_chart

   IF min(state.pt) = 32 AND max(state.pt) = 255 THEN  'Character field
    DIM d as string = charpicker
    IF d <> "" THEN
     gen(index(state.pt)) = ASC(d)
     state.need_update = YES
    END IF
   END IF
  END IF
  IF index(state.pt) THEN
   IF intgrabber(gen(index(state.pt)), min(state.pt), max(state.pt)) THEN state.need_update = YES
  END IF

  IF state.need_update THEN
   generate_battlesystem_menu menu()
   state.need_update = NO
  END IF

  clearpage vpage
  draw_fullscreen_scrollbar state, , vpage
  standardmenu menu(), state, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB statcapsmenu
 CONST maxMenu = 15
 DIM m(maxMenu) as string
 DIM max(maxMenu) as integer
 DIM index(maxMenu) as integer
 DIM state as MenuState
 state.last = maxMenu
 state.size = 24
 state.need_update = YES
 DIM i as integer

 index(1) = genDamageCap
 FOR i as integer = 2 TO 13
  index(i) = genStatCap + (i - 2)
 NEXT
 index(14) = genLevelCap
 index(15) = genMaxLevel

 max(1) = 32767
 FOR i as integer = 2 to 3 'shut up (~snicker~)
  max(i) = 9999 'HP + MP
 NEXT
 FOR i as integer = 4 to 11
  max(i) = 999 'Regular stats
 NEXT
 max(12) = 100 'MP~
 max(13) = 20  'Extra Hits
 max(14) = gen(genMaxLevel)  'Level cap is capped to Max Level
 max(15) = 99  'Max Level is capped to 99 ... FIXME: this could go higher!
 DO
  setwait 55
  setkeys

  IF keyval(scESC) > 1 OR (state.pt = 0 AND enter_or_space()) THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "stat_caps_menu"
  usemenu state
  IF state.pt > 0 THEN
   IF intgrabber(gen(index(state.pt)), 0, max(state.pt)) THEN state.need_update = YES
  END IF
  IF state.need_update THEN
   state.need_update = NO
   m(0) = "Previous Menu"
   m(1) = "Damage Cap: "
   IF gen(genDamageCap) = 0 THEN m(1) += "None" ELSE m(1) &= gen(genDamageCap)
   FOR i as integer = 0 to 11
    m(2 + i) = statnames(i) + " Cap: "
    IF gen(genStatCap + i) = 0 THEN m(2 + i) = m(2 + i) + "None" ELSE m(2 + i) = m(2 + i) & gen(genStatCap + i)
   NEXT
   IF gen(genLevelCap) > gen(genMaxLevel) THEN gen(genLevelCap) = gen(genMaxLevel)
   max(14) = gen(genMaxLevel)
   m(14) = "Initial Level Cap: " & gen(genLevelCap)
   m(15) = "Maximum Level: " & gen(genMaxLevel)
  END IF

  clearpage vpage
  standardmenu m(), state, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
END SUB

FUNCTION merge_elementals_example(byval exampleno as integer, example() as single, byval formula as integer) as string
 DIM ret as string = "Ex" & exampleno
 FOR i as integer = 0 TO 3
  DIM temp as string
  IF i >= 1 AND i <= 2 AND formula = 2 THEN  'Show equipment as additive changes
   temp = format_percent(example(i) - 1.0, 3)
   IF LEFT(temp, 1) <> "-" THEN temp = "+" + temp
  ELSE
   temp = format_percent(example(i), 3)
  END IF
  ret += RIGHT("       " + temp, 9)
 NEXT
 RETURN ret
END FUNCTION

SUB generate_equipmerge_preview(byval formula as integer, menu() as string, greyed_out() as integer, ex9() as single)
 FOR i as integer = 1 TO 3
  greyed_out(i) = YES
 NEXT
 greyed_out(1 + gen(genEquipMergeFormula)) = NO
 FOR i as integer = 4 TO UBOUND(menu)
  menu(i) = ""
 NEXT

 DIM _NaN as single = 0.0f
 _NaN = 0.0f/_NaN

 DIM ex1(3) as single = {1, 1, 3, _NaN}
 DIM ex2(3) as single = {1, 2, 2, _NaN}
 DIM ex3(3) as single = {0, 0, 1, _NaN}
 DIM ex4(3) as single = {0.5, 1, 2, _NaN}
 DIM ex5(3) as single = {-1, 1.5, 2, _NaN}
 DIM ex6(3) as single = {-1, -1, -1, _NaN}
 DIM ex7(3) as single = {2, 0.5, 0.5, _NaN}
 DIM ex8(3) as single = {1, -1.2, -1.2, _NaN}
 ex9(3) = _NaN

 IF formula = -1 THEN
  menu(9) = "Select a formula to see examples"
 ELSE
  ex1(3) = equip_elemental_merge(ex1(), formula)
  ex2(3) = equip_elemental_merge(ex2(), formula)
  ex3(3) = equip_elemental_merge(ex3(), formula)
  ex4(3) = equip_elemental_merge(ex4(), formula)
  ex5(3) = equip_elemental_merge(ex5(), formula)
  ex6(3) = equip_elemental_merge(ex6(), formula)
  ex7(3) = equip_elemental_merge(ex7(), formula)
  ex8(3) = equip_elemental_merge(ex8(), formula)
  ex9(3) = equip_elemental_merge(ex9(), formula)

  menu(9) = "Examples:"
  menu(10) = "        Hero   Equip1   Equip2   Result"
  menu(11) = merge_elementals_example(1, ex1(), formula)
  menu(12) = merge_elementals_example(2, ex2(), formula)
  menu(13) = merge_elementals_example(3, ex3(), formula)
  menu(14) = merge_elementals_example(4, ex4(), formula)
  menu(15) = merge_elementals_example(5, ex5(), formula)
  menu(16) = merge_elementals_example(6, ex6(), formula)
  menu(17) = merge_elementals_example(7, ex7(), formula)
  menu(18) = merge_elementals_example(8, ex8(), formula)
  menu(19) = merge_elementals_example(9, ex9(), formula)

  IF formula = 2 THEN
   menu(21) = "(Equipment values are displayed"
   menu(22) = "differently when this is chosen)"
  END IF
 END IF
END SUB

SUB equipmergemenu
 DIM menu(22) as string
 DIM greyed_out(22) as integer
 DIM st as MenuState
 st.size = 24
 st.last = 3
 st.need_update = YES
 DIM tog as integer

 'Random example which changes on entering the menu
 DIM ex9(3) as single = {rando(), 3*rando()-1.5, 1+rando()}

 menu(0) = "Previous Menu"
 menu(1) = "Old awful formula (multiplication-like)"
 menu(2) = "Combine resistances by multiplication"
 menu(3) = "Combine resistances by addition"
 DO
  setwait 55
  setkeys
  tog XOR= 1
  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "equip_elemental_formula"
  IF enter_or_space() THEN
   IF st.pt = 0 THEN
    EXIT DO
   ELSE
    gen(genEquipMergeFormula) = st.pt - 1
    st.need_update = YES
   END IF
  END IF
  IF usemenu(st) THEN st.need_update = YES

  IF st.need_update THEN
   generate_equipmerge_preview st.pt - 1, menu(), greyed_out(), ex9()
   st.need_update = NO
  END IF

  clearpage vpage
  FOR i as integer = 0 TO UBOUND(menu)
   IF greyed_out(i) THEN
    textcolor uilook(uiDisabledItem), 0
    IF st.pt = i THEN textcolor uilook(uiSelectedDisabled + tog), 0
   ELSE
    textcolor uilook(uiMenuItem), 0
    IF st.pt = i THEN textcolor uilook(uiSelectedItem + tog), 0
   END IF
   printstr menu(i), 0, i * 8, vpage, YES
  NEXT i
  setvispage vpage
  dowait
 LOOP
END SUB

SUB startingdatamenu
 CONST maxMenu = 4
 DIM m(maxMenu) as string
 DIM max(maxMenu) as integer
 DIM index(maxMenu) as integer
 DIM state as MenuState
 state.last = maxMenu
 state.size = 24
 state.need_update = YES
 DIM as integer lastmap = -1

 index(1) = genStartX
 index(2) = genStartY
 index(3) = genStartMap
 max(3) = gen(genMaxMap)
 index(4) = genStartMoney
 max(4) = 32767
 DO
  setwait 55
  setkeys

  IF keyval(scESC) > 1 OR (state.pt = 0 AND enter_or_space()) THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "new_game_data"
  usemenu state
  IF state.pt > 0 THEN
   IF intgrabber(gen(index(state.pt)), 0, max(state.pt)) THEN state.need_update = YES
  END IF
  IF state.need_update THEN
   state.need_update = NO
   IF lastmap <> gen(genStartMap) THEN
    DIM fh as integer
    fh = FREEFILE
    OPEN maplumpname(gen(genStartMap), "t") FOR BINARY as #fh
    SEEK #fh, 8
    max(1) = Readshort(fh, -1) - 1 'map width
    max(2) = ReadShort(fh, -1) - 1 'map height
    CLOSE #fh
    gen(genStartX) = small(gen(genStartX), max(1))
    gen(genStartY) = small(gen(genStartY), max(2))
    lastmap = gen(genStartMap)
   END IF

   m(0) = "Previous Menu"
   m(1) = "Starting X: " & gen(genStartX)
   m(2) = "Starting Y: " & gen(genStartY)
   m(3) = "Starting Map: " & gen(genStartMap) & " " & getmapname(gen(genStartMap))
   m(4) = "Starting Money: " & gen(genStartMoney)
  END IF

  clearpage vpage
  standardmenu m(), state, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB generate_gen_menu(m() as string, longname as string, aboutline as string)
 m(1) = "Long Name:" + longname
 m(2) = "About Line:" + aboutline
 IF gen(genMaxInventory) = 0 THEN
  m(12) = "Inventory size: Default (" & (last_inv_slot() \ 3) + 1 & " rows)"
 ELSE
  m(12) = "Inventory size: " & (Last_inv_slot() \ 3) + 1 & " rows, " & gen(genMaxInventory) + 1 & " slots"
 END IF
 m(13) = "Inventory autosort: "
 SELECT CASE gen(genAutosortScheme)
  CASE 0: m(13) += "by item type/uses"
  CASE 1: m(13) += "by whether usable"
  CASE 2: m(13) += "alphabetically"
  CASE 3: m(13) += "by item ID number"
  CASE 4: m(13) += "no reordering"
 END SELECT
 m(14) = "Script errors: "
 SELECT CASE gen(genErrorLevel)
  CASE 2: m(14) += "Show all warnings"
  CASE 3: m(14) += "Hide nit-picking warnings"
  CASE 4: m(14) += "Hide all warnings"
  CASE 5: m(14) += "Hide errors not reported in old versions"
  CASE 6: m(14) += "Hide all ignoreable errors"
 END SELECT
END SUB

SUB gendata ()
 CONST maxMenu = 14
 DIM m(maxMenu) as string
 DIM min(maxMenu) as integer
 DIM max(maxMenu) as integer
 DIM index(maxMenu) as integer
 DIM enabled(maxMenu) as integer

 DIM state as MenuState
 WITH state
  .size = 24
  .last = maxMenu
  .need_update = YES
 END WITH

 'make sure genMaxInventory is a valid value (possible in older versions)
 IF gen(genMaxInventory) THEN gen(genMaxInventory) = last_inv_slot()
 
 m(0) = "Return to Main Menu"
 m(3) = "Preference Bitsets..."
 m(4) = "Pick Title Screen..."
 m(5) = "New Game Settings..."
 m(6) = "Special Plotscripts..."
 m(7) = "Battle System Options..."
 m(8) = "Global Music and Sound Effects..."
 m(9) = "Master Palettes..."
 m(10) = "Password For Editing..."

 flusharray enabled(), UBOUND(enabled), YES
 enabled(11) = NO
 index(12) = genMaxInventory
 max(12) = (inventoryMax + 1) \ 3
 index(13) = genAutosortScheme
 max(13) = 4
 index(14) = genErrorLevel
 max(14) = 6
 min(14) = 2

 DIM aboutline as string = load_aboutline()
 DIM longname as string = load_gamename()

 setkeys YES
 DO
  setwait 55
  setkeys YES

  IF state.need_update THEN
   generate_gen_menu m(), longname, aboutline
   state.need_update = NO
  END IF

  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "general_game_data"
  usemenu state, enabled()
  IF enter_or_space() THEN
   IF state.pt = 0 THEN EXIT DO
   IF state.pt = 3 THEN
    DIM bittemp(2) as integer
    DIM bitname(32) as string
    bitname(0) = "Pause on Battle Sub-menus"
    bitname(1) = "Enable Caterpillar Party"
    bitname(2) = "Don't Restore HP on Levelup"
    bitname(3) = "Don't Restore MP on Levelup"
    bitname(4) = "Inns Don't Revive Dead Heroes"
    bitname(5) = "Hero Swapping Always Available"
    bitname(6) = "Hide Ready-meter in Battle"
    bitname(7) = "Hide Health-meter in Battle"
    bitname(8) = "Disable Debugging Keys"
    bitname(9) = "Simulate Old Levelup Bug"
    bitname(10) = "Permit double-triggering of scripts"
    bitname(11) = "Skip title screen"
    bitname(12) = "Skip load screen"
    bitname(13) = "Pause on All Battle Menus"
    bitname(14) = "Disable Hero's Battle Cursor"
    bitname(15) = "Default passability disabled by default"
    bitname(16) = "Simulate Pushable NPC obstruction bug"
    bitname(17) = "Disable ESC key running from battle"
    bitname(18) = "Don't save gameover/loadgame script IDs"
    bitname(19) = "Dead heroes gain share of experience"
    bitname(20) = "Locked heroes can't be re-ordered"
    bitname(21) = "Attack captions pause battle meters"
    bitname(22) = "Don't randomize battle ready meters"
    bitname(23) = "Battle menus wait for attack animations"
    bitname(24) = "Enable better scancodes for scripts"
    bitname(25) = "Simulate old fail vs element resist bit"
    bitname(26) = "0 damage when immune to attack elements"
    bitname(27) = "Recreate map slices when changing maps"
    bitname(28) = "Harm tiles harm non-caterpillar heroes"
    bitname(29) = "Attacks will ignore extra hits stat"
    bitname(30) = "Don't divide experience between heroes"
    bitname(31) = "Don't reset max stats after OOB attack"
    bitname(32) = "Don't limit maximum tags to 999"
    bittemp(0) = gen(genBits)
    bittemp(1) = gen(genBits2)
    bittemp(2) = gen(genBits2+1)
    editbitset bittemp(), 0, UBOUND(bitname), bitname(), "general_game_bitsets"
    gen(genBits) = bittemp(0)
    gen(genBits2) = bittemp(1)
    gen(genBits2+1) = bittemp(2)
   END IF
   IF state.pt = 4 THEN titlescreenbrowse
   IF state.pt = 5 THEN startingdatamenu
   IF state.pt = 6 THEN generalscriptsmenu
   IF state.pt = 7 THEN battleoptionsmenu
   IF state.pt = 8 THEN generalmusicsfxmenu
   IF state.pt = 9 THEN masterpalettemenu
   IF state.pt = 10 THEN inputpasw
  END IF
  IF state.pt = 1 THEN
   IF strgrabber(longname, 38) THEN state.need_update = YES
  ELSEIF state.pt = 2 THEN
   IF strgrabber(aboutline, 38) THEN state.need_update = YES
  ELSEIF index(state.pt) = genMaxInventory THEN
   DIM as integer temp = (gen(genMaxInventory) + 1) \ 3
   IF intgrabber(temp, min(state.pt), max(state.pt)) THEN
    gen(genMaxInventory) = temp * 3 - 1
    IF temp = 0 THEN gen(genMaxInventory) = 0
    state.need_update = YES
   END IF
  ELSEIF index(state.pt) THEN
   IF intgrabber(gen(index(state.pt)), min(state.pt), max(state.pt)) THEN state.need_update = YES
  END IF

  clearpage dpage
  draw_fullscreen_scrollbar state, , dpage
  standardmenu m(), state, 0, 0, dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 
 '--write long name and about line
 save_gamename longname
 save_aboutline aboutline
END SUB
