'OHRRPGCE - music and sound effects editors & importing
'(C) Copyright 2017 James Paige/OHRRPGCE developers
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "customsubs.bi"
#include "cglobals.bi"
#include "custom.bi"
#include "const.bi"
#include "uiconst.bi"
#include "loading.bi"


'''' Local functions

DECLARE SUB import_convert_mp3(byref mp3 as string, byref oggtemp as string)
DECLARE SUB import_convert_wav(byref wav as string, byref oggtemp as string)

DECLARE SUB delete_song (byval songnum as integer, songfile as string, bamfile as string)
DECLARE SUB importsong_save_song_data(songname as string, byval songnum as integer)
DECLARE SUB importsong_exportsong(songfile as string, bamfile as string, file_ext as string, songname as string)
DECLARE SUB importsong_get_song_info (songname as string, songfile as string, bamfile as string, byval songnum as integer, file_ext as string, menu() as string, metadata as string, selectable() as bool, state as MenuState)
DECLARE SUB importsong_import_song_file (songname as string, songfile as string, bamfile as string, byval songnum as integer)
DECLARE SUB importsfx_get_sfx_info(sfxname as string, sfxfile as string, byval sfxnum as integer, file_ext as string, menu() as string, metadata as string, selectable() as bool, state as MenuState)
DECLARE SUB importsfx_save_sfx_data(sfxname as string, byval sfxnum as integer)
DECLARE SUB importsfx_exportsfx(sfxfile as string, file_ext as string, sfxname as string)
DECLARE SUB importsfx_importsfxfile(sfxname as string, sfxfile as string, byval sfxnum as integer, file_ext as string)


'==========================================================================================
'                                Audio format conversion
'==========================================================================================


SUB import_convert_mp3(byref mp3 as string, byref oggtemp as string)
 DIM ogg_quality as integer
 IF pick_ogg_quality(ogg_quality) = NO THEN mp3 = "" : EXIT SUB
 oggtemp = tmpdir & "temp." & randint(100000) & ".ogg"
 clearpage vpage
 basic_textbox "Please wait, converting to OGG...", uilook(uiText), vpage
 setvispage vpage, NO
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
 IF pick_ogg_quality(ogg_quality) = NO THEN wav = "" : EXIT SUB
 oggtemp = tmpdir & "temp." & randint(100000) & ".ogg"
 clearpage vpage
 basic_textbox "Please wait, converting to OGG...", uilook(uiText), vpage
 setvispage vpage, NO
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


'==========================================================================================
'                                  Music Import/Editor
'==========================================================================================


SUB importsong ()
REDIM menu(6) as string
REDIM selectable(6) as bool
menu(0) = "Previous Menu"
menu(3) = "Import Song..."
menu(4) = "Export Song..."
menu(5) = "Delete Song"

DIM state as MenuState
state.size = 24
state.autosize = YES
state.autosize_ignore_lines = 1
state.pt = 1

DIM songnum as integer = 0
DIM songname as string = ""
DIM songfile as string = ""
DIM bamfile as string = ""  '"" if none, differs from songfile if it's a BAM fallback
DIM file_ext as string
DIM metadata as string
importsong_get_song_info songname, songfile, bamfile, songnum, file_ext, menu(), metadata, selectable(), state

setkeys YES
DO
 setwait 55
 setkeys YES
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "import_songs"
 IF keyval(scF2) > 1 THEN Custom_volume_menu
 IF keyval(scF3) > 1 THEN music_backend_menu

 usemenu state, selectable()

 IF state.pt = 2 AND songfile <> "" THEN
  strgrabber songname, 30
  menu(2) = "Name: " + songname
 ELSE
  '-- check for switching song
  DIM newsong as integer = songnum
  IF intgrabber(newsong, 0, gen(genMaxSong), scLeftCaret, scRightCaret) THEN
   importsong_save_song_data songname, songnum
   songnum = newsong
   state.need_update = YES
  END IF
  IF keyval(scLeft) > 1 AND songnum > 0 THEN
   importsong_save_song_data songname, songnum
   songnum -= 1
   state.need_update = YES
  END IF
  IF keyval(scRight) > 1 AND songnum < 32767 THEN
   importsong_save_song_data songname, songnum
   songnum += 1
   IF needaddset(songnum, gen(genMaxSong), "song") THEN songname = ""
   state.need_update = YES
  END IF
 END IF
 IF enter_space_click(state) THEN
  IF state.pt = 0 THEN EXIT DO
  IF state.pt = 3 THEN
   importsong_import_song_file songname, songfile, bamfile, songnum
   state.need_update = YES
  END IF
  IF state.pt = 4 AND songfile <> "" THEN importsong_exportsong songfile, bamfile, file_ext, songname
  IF state.pt = 5 AND songfile <> "" THEN  'delete song
   IF yesno("Really delete this song?", NO, NO) THEN
    music_stop
    'closemusic  'music_stop not always enough to cause the music backend to let go of the damn file!
    'setupmusic
    delete_song songnum, songfile, bamfile
    state.need_update = YES
   END IF
  END IF
  IF state.pt = 6 THEN  'delete BAM fallback
   IF yesno("Really delete this BAM song?", NO, NO) THEN
    safekill bamfile
    state.need_update = YES
    state.pt = 0
   END IF
  END IF
 END IF

 IF state.need_update THEN
  state.need_update = NO
  importsong_get_song_info songname, songfile, bamfile, songnum, file_ext, menu(), metadata, selectable(), state
 END IF

 clearpage dpage
 standardmenu menu(), state, 0, 0, dpage
 wrapprint metadata, 0, 9 * (UBOUND(menu) + 2), uilook(uiMenuItem), dpage
 edgeprint "F2: change preview volume", pRight, pBottom, uilook(uiMenuItem), dpage

 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
importsong_save_song_data songname, songnum
music_stop
EXIT SUB

END SUB

SUB importsong_import_song_file (songname as string, songfile as string, bamfile as string, byval songnum as integer)
 STATIC default as string
 music_stop
 'closemusic  'music_stop not always enough to cause the music backend to let go of the damn file!
 'setupmusic

 'browse for new song
 DIM sourcesong as string = browse(5, default, "", "browse_import_song")

 'Get song name
 DIM newname as string = decode_filename(trimextension(trimpath(sourcesong)))

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

 delete_song songnum, songfile, bamfile

 songname = newname

 'generate lump name
 DIM extension as string = LCASE(justextension(sourcesong))
 IF extension = "bam" AND songnum <= 99 THEN
  songfile = game + "." & songnum
 ELSE
  songfile = workingdir & SLASH & "song" & songnum & "." & extension
 END IF

 'Copy in new lump (this implicitly sends a notification to Game if it's been spawned)
 copyfile sourcesong, songfile

 IF oggtemp <> "" THEN killfile oggtemp

 importsong_save_song_data songname, songnum
END SUB

SUB importsong_get_song_info (songname as string, songfile as string, bamfile as string, byval songnum as integer, file_ext as string, menu() as string, metadata as string, selectable() as bool, state as MenuState)
 music_stop

 DIM temp as string
 '-- first job: find the song's name
 temp = workingdir & SLASH & "song" & songnum
 songfile = ""
 file_ext = ""
 DIM songtype as string = "NO FILE"
 '-- BAM special case and least desirable, so check first and override
 IF songnum > 99 THEN
  IF isfile(temp & ".bam") THEN
   file_ext = ".bam"
   songfile = temp & file_ext
   songtype = "Bob's Adlib Music (BAM)"
  END IF
 ELSE
  IF isfile(game & "." & songnum) THEN
   file_ext = ".bam"
   songfile = game & "." & songnum
   songtype = "Bob's Adlib Music (BAM)"
  END IF
 END IF
 bamfile = songfile

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

 songname = getsongname(songnum)

 IF songfile <> "" THEN '--song exists
  loadsong songfile
 ELSE
  songname = ""
 END IF

 DIM optionsbottom as integer

 menu(1) = "<- Song " & songnum & " of " & gen(genMaxSong) & " ->"
 IF songfile <> "" THEN menu(2) = "Name: " & songname ELSE menu(2) = "-Unused-"
 IF bamfile <> songfile AND bamfile <> "" THEN
  menu(6) = "Delete BAM fallback"
  optionsbottom = 6
 ELSE
  menu(6) = ""
  optionsbottom = 5
 END IF

 metadata  = "Type:     " & songtype & !"\n"
 metadata &= "Filesize: " & filesize(songfile) & !"\n"
 IF bamfile <> songfile AND bamfile <> "" THEN
  metadata &= "BAM fallback exists. Filesize: " & filesize(bamfile) & !"\n"
 END IF

 '-- add author, length, etc, info here
 IF file_ext = ".ogg" THEN
  metadata &= read_ogg_metadata(songfile)
 END IF

 REDIM PRESERVE selectable(UBOUND(menu))
 FOR i as integer = 0 TO UBOUND(selectable)
  selectable(i) = (i <= optionsbottom)
 NEXT

 state.last = UBOUND(menu)
END SUB

'songfile: Complete path to the song file to be exported
'bamfile: The .bam fallback file, if any
SUB importsong_exportsong(songfile as string, bamfile as string, file_ext as string, songname as string)
 IF bamfile <> songfile AND LEN(bamfile) THEN
  DIM choice as integer = twochoice("Export which version of this song?", _
                                    file_ext + " file", _
                                    ".bam fallback file")
  IF choice = 1 THEN file_ext = ".bam" : songfile = bamfile
  IF choice = -1 THEN EXIT SUB
 END IF
 DIM query as string = "Name of file to export to?"
 DIM outfile as string = inputfilename(query, file_ext, "", "input_file_export_song", songname)
 IF outfile = "" THEN EXIT SUB
 copyfile songfile, outfile & file_ext
END SUB

SUB delete_song (byval songnum as integer, songfile as string, bamfile as string)
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
 IF LEN(bamfile) THEN safekill bamfile
 'FIXME: handle deleting from rpgdirs (bug 247)... and the same for soundeffects

 IF slave_channel <> NULL_CHANNEL THEN send_lump_modified_msg(songfile)  'only need to send any valid filename for this song
END SUB

SUB importsong_save_song_data(songname as string, byval songnum as integer)
 DIM songbuf(dimbinsize(binSONGDATA)) as integer
 writebinstring songname, songbuf(), 0, 30
 storerecord songbuf(), workingdir & SLASH & "songdata.bin", curbinsize(binSONGDATA) \ 2, songnum
END SUB


'==========================================================================================
'                                Sound Effects Import/Editor
'==========================================================================================


SUB importsfx ()

REDIM menu(6) as string
REDIM selectable(6) as bool  'No actual purpose
menu(0) = "Previous Menu"
menu(3) = "Import Sound..."
menu(4) = "Export Sound..."
menu(5) = "Delete Sound"
menu(6) = "Play Sound"

DIM state as MenuState
state.pt = 1
state.size = 24
state.autosize = YES
state.autosize_ignore_lines = 1
state.last = UBOUND(menu)

DIM sfxnum as integer = 0
DIM sfxname as string = ""
DIM sfxfile as string = ""
DIM newsfx as integer
DIM file_ext as string
DIM metadata as string
importsfx_get_sfx_info sfxname, sfxfile, sfxnum, file_ext, menu(), metadata, selectable(), state

setkeys YES
DO
 setwait 55
 setkeys YES
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "import_sfx"
 IF keyval(scF2) > 1 THEN Custom_volume_menu
 IF keyval(scF3) > 1 THEN music_backend_menu

 usemenu state, selectable()

 IF state.pt = 2 AND sfxfile <> "" THEN
  strgrabber sfxname, 30
  menu(2) = "Name: " + sfxname
 ELSE
  '-- check for switching sfx
  newsfx = sfxnum
  IF intgrabber(newsfx, 0, gen(genMaxSFX), scLeftCaret, scRightCaret) THEN
   importsfx_save_sfx_data sfxname, sfxnum
   freesfx sfxnum
   sfxnum = newsfx
   importsfx_get_sfx_info sfxname, sfxfile, sfxnum, file_ext, menu(), metadata, selectable(), state
  END IF
  IF keyval(scLeft) > 1 AND sfxnum > 0 THEN
   importsfx_save_sfx_data sfxname, sfxnum
   freesfx sfxnum
   sfxnum -= 1
   importsfx_get_sfx_info sfxname, sfxfile, sfxnum, file_ext, menu(), metadata, selectable(), state
  END IF
  IF keyval(scRight) > 1 AND sfxnum < 32767 THEN
   importsfx_save_sfx_data sfxname, sfxnum
   freesfx sfxnum
   sfxnum += 1
   IF needaddset(sfxnum, gen(genMaxSFX), "sfx") THEN sfxname = ""
   importsfx_get_sfx_info sfxname, sfxfile, sfxnum, file_ext, menu(), metadata, selectable(), state
  END IF
 END IF
 IF enter_space_click(state) THEN
  SELECT CASE state.pt
  CASE 0   'quit
    EXIT DO
  CASE 3   'import
    freesfx sfxnum
    importsfx_importsfxfile sfxname, sfxfile, sfxnum, file_ext
    importsfx_get_sfx_info sfxname, sfxfile, sfxnum, file_ext, menu(), metadata, selectable(), state
  CASE 4   'export
    IF sfxfile <> "" THEN importsfx_exportsfx sfxfile, file_ext, sfxname
  CASE 5   'delete sfx
    IF sfxfile <> "" THEN
      IF yesno("Really delete this sound?", NO, NO) THEN
        freesfx sfxnum
        safekill sfxfile
        importsfx_get_sfx_info sfxname, sfxfile, sfxnum, file_ext, menu(), metadata, selectable(), state
      END IF
    END IF
  CASE 1, 6
    IF sfxfile <> "" THEN 'play sfx
      playsfx sfxnum, 0
    END IF

  END SELECT
 END IF

 clearpage dpage
 standardmenu menu(), state, 0, 0, dpage
 wrapprint metadata, 0, 9 * (UBOUND(menu) + 2), uilook(uiMenuItem), dpage
 edgeprint "F2: change preview volume", pRight, pBottom, uilook(uiMenuItem), dpage

 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
importsfx_save_sfx_data sfxname, sfxnum
freesfx sfxnum
END SUB

SUB importsfx_importsfxfile(sfxname as string, sfxfile as string, byval sfxnum as integer, file_ext as string)
 STATIC default as string

 DIM sourcesfx as string = browse(6, default, "", "browse_import_sfx")

 '-- get name (before sourcesfx is modified)
 DIM newname as string = decode_filename(trimextension(trimpath(sourcesfx)))

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

 'Delete the old file. We cannot assume it will be overwritten because the extension might change
 IF sfxfile <> "" THEN safekill sfxfile

 sfxname = newname

 '-- calculate lump name
 sfxfile = workingdir & SLASH & "sfx" & sfxnum & "." & LCASE(justextension(sourcesfx))

 '--copy in the new lump
 copyfile sourcesfx, sfxfile

 IF oggtemp <> "" THEN killfile oggtemp

 '--save and update
 importsfx_save_sfx_data sfxname, sfxnum
END SUB

SUB importsfx_exportsfx(sfxfile as string, file_ext as string, sfxname as string)
 DIM query as string = "Name of file to export to?"
 DIM outfile as string = inputfilename(query, file_ext, "", "input_file_export_sfx", sfxname)
 IF outfile = "" THEN EXIT SUB
 copyfile sfxfile, outfile & file_ext
END SUB

SUB importsfx_save_sfx_data(sfxname as string, byval sfxnum as integer)
 DIM sfxbuf(dimbinsize(binSFXDATA)) as integer
 writebinstring sfxname, sfxbuf(), 0, 30
 storerecord sfxbuf(), workingdir & SLASH & "sfxdata.bin", curbinsize(binSFXDATA) \ 2, sfxnum
END SUB

SUB importsfx_get_sfx_info(sfxname as string, sfxfile as string, byval sfxnum as integer, file_ext as string, menu() as string, metadata as string, selectable() as bool, state as MenuState)
 '-- first job: find the sfx's name
 DIM temp as string = workingdir & SLASH & "sfx" & sfxnum
 DIM sfxtype as string = "NO FILE"
 
 sfxfile = "" ' this will be rebuilt below

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
  'playsfx sfxnum, 0
  sfxname = getsfxname(sfxnum)
 ELSE '--sfx doesn't exist
  sfxname = ""
 END IF

 menu(1) = "<- SFX " & sfxnum & " of " & gen(genMaxSFX) & " ->"
 IF sfxfile <> "" THEN menu(2) = "Name: " & sfxname ELSE menu(2) = "-Unused-"

 metadata  = "Type:     " & sfxtype & !"\n"
 metadata &= "Filesize: " & filesize(sfxfile) & !"\n"

 '-- add author, length, etc, info here
 IF file_ext = ".ogg" THEN
  metadata &= read_ogg_metadata(sfxfile)
 END IF

 REDIM selectable(UBOUND(menu))
 FOR i as integer = 0 TO 6
  selectable(i) = YES
 NEXT
 state.last = UBOUND(menu)
END SUB


'==========================================================================================
'                                  Global Music/Sounds
'==========================================================================================

SUB generalmusicsfxmenu ()
  CONST menusize as integer = 18
  CONST lastmusicitem as integer = 3
  DIM as string menu(menusize), disp(menusize), menu_display(menusize)
  DIM as integer index(1 to menusize) = { _
          genTitleMus, genBatMus, genVictMus, genAcceptSFX, genCancelSFX, _
          genCursorSFX, genTextboxLine, genDefaultDeathSFX, genItemLearnSFX, genCantLearnSFX, _
          genBuySFX, genHireSFX, genSellSFX, genCantBuySFX, genCantSellSFX, _
          genStealSuccessSFX, genStealFailSFX, genStealNoItemSFX _
  }

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
  menu(16) = "Steal Success Sound: "
  menu(17) = "Steal Failure Sound: "
  menu(18) = "Steal No-Item Sound: "

  DIM selectst as SelectTypeState
  DIM state as MenuState
  state.size = 24
  state.last = menusize
  state.need_update = YES

  setkeys YES
  DO
    setwait 55
    setkeys YES

    IF keyval(scESC) > 1 THEN EXIT DO
    IF keyval(scF1) > 1 THEN show_help "general_music_sfx"
    IF keyval(scF2) > 1 THEN Custom_volume_menu
    usemenu state

    IF enter_space_click(state) THEN
      SELECT CASE state.pt
      CASE 0
        EXIT DO
      CASE 1 TO lastmusicitem
        IF gen(index(state.pt)) > 0 THEN playsongnum gen(index(state.pt)) - 1
      CASE lastmusicitem + 1 TO state.last
        IF gen(index(state.pt)) > 0 THEN playsfx gen(index(state.pt)) - 1
      END SELECT
    END IF

    SELECT CASE state.pt
    CASE 1 TO lastmusicitem
      IF zintgrabber(gen(index(state.pt)), -1, gen(genMaxSong)) THEN
        music_stop
        state.need_update = YES
      END IF
    CASE lastmusicitem + 1 TO state.last
      IF zintgrabber(gen(index(state.pt)), -1, gen(genMaxSFX)) THEN
        resetsfx
        state.need_update = YES
      END IF
    END SELECT

    IF state.need_update THEN
      state.need_update = NO
      FOR idx as integer = 1 to state.last
        DIM value as integer = gen(index(idx)) - 1
        IF value >= 0 THEN
          IF idx <= lastmusicitem THEN
            disp(idx) = menu(idx) & getsongname(value, -1)  'prefixes number
          ELSE
            disp(idx) = menu(idx) & value & " " & getsfxname(value)
          END IF
        ELSE
          disp(idx) = menu(idx) & "None"
        END IF
      NEXT
    END IF    

    IF select_by_typing(selectst, NO) THEN
      select_on_word_boundary disp(), selectst, state
    END IF

    clearpage dpage
    highlight_menu_typing_selection disp(), menu_display(), selectst, state
    standardmenu menu_display(), state, 0, 0, dpage 
    edgeprint "F2: change preview volume", pRight, pBottom, uilook(uiMenuItem), dpage

    SWAP vpage, dpage
    setvispage vpage
    dowait
  LOOP
  music_stop
  resetsfx
END SUB
