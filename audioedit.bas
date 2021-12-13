'OHRRPGCE - music and sound effects editors & importing
'(C) Copyright 2017 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "customsubs.bi"
#include "cglobals.bi"
#include "custom.bi"
#include "const.bi"
#include "uiconst.bi"
#include "loading.bi"
#include "thingbrowser.bi"


'''' Local functions

DECLARE FUNCTION mp3_to_ogg (in_file as string, out_file as string, quality as integer = 4, filter_high_freq as bool = YES) as string
DECLARE FUNCTION mp3_to_wav (in_file as string, out_file as string) as string
DECLARE FUNCTION wav_to_ogg (in_file as string, out_file as string, quality as integer = 4, filter_high_freq as bool = YES, comments() as string) as string

DECLARE SUB export_songlist()

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
'                                   Audio re-encoding
'==========================================================================================


FUNCTION find_madplay () as string
 STATIC cached as bool = NO
 STATIC cached_app as string
 IF cached THEN RETURN cached_app
 cached_app = find_helper_app("madplay", YES)
 cached = YES
 RETURN cached_app
END FUNCTION

FUNCTION find_oggenc () as string
 STATIC cached as bool = NO
 STATIC cached_app as string
 IF cached THEN RETURN cached_app
 cached_app = find_helper_app("oggenc", YES)
 IF cached_app = "" THEN cached_app = find_helper_app("oggenc2")
 cached = YES
 RETURN cached_app
END FUNCTION

'Read ID3 tags from an mp3 and return them as a list of strings in the form "key=value"
'NOTE: This invokes madplay to get the ID3 tags, but we already have
'read_mp3_metadata(), which could be extended to the do the same without madplay.
'However, madplay does translated various ID codes like "TPUB" to "Publisher" for us.
SUB mp3_ID3_tags (in_file as string, comments() as string)
 ERASE comments

 DIM as string app, args
 app = find_madplay()
 'Shouldn't happen
 IF app = "" THEN
  visible_debug "Can not read MP3 files: " + missing_helper_message("madplay" + DOTEXE)
  EXIT SUB
 END IF

 'We throw away encoder comments (-v)
 args = " -T -q " & escape_filename(in_file)

 DIM errcode as integer
 DIM as string stdout_s, stderr_s
 errcode = run_and_get_output(escape_filename(app) & args, stdout_s, stderr_s)
 IF errcode THEN
  visible_debug "Couldn't get ID3 tags from MP3 file: " & stderr_s
  EXIT SUB
 END IF

 'madplay actually prints all the tag info to stderr...
 DIM lines() as string
 split stderr_s, lines()

 'Each line is of the form "           Year: 2009"
 'but might be word-wrapped.
 'Also the "Copyright (C)" and "Produced (P)" lines are
 'are exceptions for some reason (they have no 'Key:' part), and it looks like there
 'can be other tags with no 'key' too.
 FOR idx as integer = 0 TO UBOUND(lines)
  DIM tag as string = TRIM(lines(idx))
  IF LEN(tag) = 0 THEN CONTINUE FOR
  DIM colon as integer = INSTR(tag, ": ")
  DIM as string key, text

  'Copyright and Produced special cases (start with 17 spaces)
  IF INSTR(tag, "Copyright ") = 1 THEN
   key = "Copyright"
   text = MID(tag, 11)
  ELSEIF INSTR(tag, "Produced ") = 1 THEN
   key = "Produced"
   text = MID(tag, 10)
  ELSEIF colon = 0 THEN
   'Other lines are generally garbage, a repeat of the previous line
   CONTINUE FOR
  ELSE
   key = LEFT(tag, colon - 1)
   text = MID(tag, colon + 2)
  END IF

  'Concatenate apparent wrapped lines
  WHILE LEN(lines(idx)) > 65 ANDALSO idx + 1 <= UBOUND(lines) ANDALSO LEFT(lines(idx + 1), 17) = SPACE(17)
   idx += 1
   text += " " + TRIM(lines(idx))
  WEND

  IF key = "Encoder" THEN CONTINUE FOR  'We're reencoding, ditch that

  a_append comments(), key & "=" & text
 NEXT
END SUB

'Returns error message, or "" on success
FUNCTION mp3_to_ogg (in_file as string, out_file as string, quality as integer = 4, filter_high_freq as bool = YES) as string
 DIM as string tempwav
 DIM as string ret
 tempwav = tmpdir & "temp." & randint(100000) & ".wav"
 ret = mp3_to_wav(in_file, tempwav)
 IF LEN(ret) THEN RETURN ret
 DIM comments() as string
 mp3_ID3_tags in_file, comments()
 ret = wav_to_ogg(tempwav, out_file, quality, filter_high_freq, comments())
 safekill tempwav
 RETURN ret
END FUNCTION

'Returns error message, or "" on success
FUNCTION mp3_to_wav (in_file as string, out_file as string) as string
 DIM as string app, args, ret
 IF NOT isfile(in_file) THEN RETURN "mp3 to wav conversion: " & in_file & " does not exist"
 app = find_madplay()
 IF app = "" THEN RETURN "Can not read MP3 files: " + missing_helper_message("madplay" + DOTEXE)

 args = " -o wave:" & escape_filename(out_file) & " " & escape_filename(in_file)
 ret = spawn_and_wait(app, args)
 IF LEN(ret) THEN
  safekill out_file
  RETURN ret
 END IF

 IF NOT isfile(out_file) THEN RETURN "Could not find " + out_file + ": " + app + " must have failed"
 RETURN ""
END FUNCTION

'Returns error message, or "" on success
FUNCTION wav_to_ogg (in_file as string, out_file as string, quality as integer = 4, filter_high_freq as bool = YES, comments() as string) as string
 DIM as string app, args, ret
 IF NOT isfile(in_file) THEN RETURN "wav to ogg conversion: " & in_file & " does not exist"
 app = find_oggenc()
 IF app = "" THEN RETURN "Can not convert to OGG: " + missing_helper_message("oggenc" DOTEXE " and oggenc2" DOTEXE)

 args = " -q " & quality & " -o " & escape_filename(out_file) & " " & escape_filename(in_file)
 IF filter_high_freq = NO THEN
  args &= " --advanced-encode-option lowpass_frequency=22"
 END IF
 FOR idx as integer = 0 TO UBOUND(comments)
  args &= " -c " & escape_filename(comments(idx))
 NEXT
 ret = spawn_and_wait(app, args)
 IF LEN(ret) THEN
  safekill out_file
  RETURN "wav to ogg conversion failed: " & ret
 END IF

 IF NOT isfile(out_file) THEN RETURN "Could not find " + out_file + ": " + app + " must have failed"
 RETURN ""
END FUNCTION


'==========================================================================================
'                                Audio re-encoding menus
'==========================================================================================


'Returns true and sets 'quality' if not cancelled
FUNCTION pick_ogg_quality(byref quality as integer, byref filter_high_freq as bool) as bool
 filter_high_freq = YES
 STATIC q as integer = 2
 DIM i as integer
 DIM descrip as string
 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(ccCancel) > 1 THEN
   RETURN NO  'cancelled
  END IF
  IF keyval(scF1) > 1 THEN show_help "pick_ogg_quality"
  IF keyval(scTab) > 1 THEN
   filter_high_freq = yesno("Filter out high-pitched noise? YES is default, NO increases file size " _
                            "without improving audio quality, but suitable for hissing noise sfx", YES, YES)
  END IF
  IF enter_or_space() THEN EXIT DO
  CONST MINQ = -1
  CONST MAXQ = 9
  intgrabber q, MINQ, MAXQ
  clearpage vpage
  centerbox rCenter, 105, 300, 54, 4, vpage
  'We don't know the number of channels, so assume 2...
  edgeprint "Pick Ogg quality level: " & q & " (~" & oggenc_quality_levels(2, q) & "kbps)", pCentered, 86, uilook(uiText), vpage
  CONST thipsize = 23
  FOR i = 0 TO q + 1
   rectangle rCenter - ((MAXQ - MINQ + 1) * thipsize) \ 2 + thipsize * i, 100, thipsize - 1, 16, uilook(uiText), vpage
  NEXT i
  SELECT CASE q
   CASE -1: descrip = "scratchy, smallest"
   CASE 0: descrip = "not too bad, very small"
   CASE 1: descrip = "pretty good, quite small"
   CASE 2: descrip = "good, pretty small"
   CASE 3: descrip = "good, smallish"
   CASE 4: descrip = "good, medium sized"
   CASE 5: descrip = "good, biggish"
   CASE 6: descrip = "better than you need, big"
   CASE 7: descrip = "much better than you need, too big"
   CASE 8: descrip = "excessive, wasteful"
   CASE 9: descrip = "very excessive, very wasteful"
   CASE 10: descrip = "flagrantly excessive and wasteful"
  END SELECT
  edgeprint descrip, pCentered, 118, uilook(uiText), vpage
  edgeprint "TAB: advanced options", pLeft, pBottom, uilook(uiMenuItem), vpage
  setvispage vpage
  dowait
 LOOP
 quality = q
 RETURN YES
END FUNCTION

SUB import_convert_mp3(byref mp3 as string, byref oggtemp as string)
 DIM ogg_quality as integer
 DIM filter_high_freq as bool
 IF pick_ogg_quality(ogg_quality, filter_high_freq) = NO THEN mp3 = "" : EXIT SUB
 oggtemp = tmpdir & "temp." & randint(100000) & ".ogg"
 clearpage vpage
 basic_textbox "Please wait, converting to OGG...", uilook(uiText), vpage
 setvispage vpage, NO
 DIM ret as string = mp3_to_ogg(mp3, oggtemp, ogg_quality, filter_high_freq)
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
 DIM filter_high_freq as bool
 IF pick_ogg_quality(ogg_quality, filter_high_freq) = NO THEN wav = "" : EXIT SUB
 oggtemp = tmpdir & "temp." & randint(100000) & ".ogg"
 clearpage vpage
 basic_textbox "Please wait, converting to OGG...", uilook(uiText), vpage
 setvispage vpage, NO
 'WAV files CAN contain metadata and even ID3 tags, but we don't support them yet
 DIM comments() as string
 DIM ret as string = wav_to_ogg(wav, oggtemp, ogg_quality, filter_high_freq, comments())
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

SUB song_editor_main ()
 IF read_config_bool("thingbrowser.enable_top_level", YES) THEN
  DIM b as SongBrowser
  b.browse(-1, , @importsong)
 ELSE
  importsong 0
 END IF
 music_stop
END SUB

FUNCTION song_picker (recindex as integer = -1) as integer
 DIM b as SongBrowser
 RETURN b.browse(recindex, , @importsong, NO)
END FUNCTION

FUNCTION song_picker_or_none (recindex as integer = -1) as integer
 DIM b as SongBrowser
 RETURN b.browse(recindex - 1, YES , @importsong, NO) + 1
END FUNCTION

FUNCTION importsong (byval songnum as integer) as integer
'songnum is the song to start with, or > max to add a new one.
'Return value is the last selected song, or -1 if cancelling an add-new

IF songnum > gen(genMaxSong) THEN
 'Requested a new one
 IF NOT needaddset(songnum, gen(genMaxSong), "song") THEN
  'Cancelled
  RETURN -1
 END IF
 importsong_save_song_data "", songnum
END IF

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
 IF keyval(ccCancel) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "import_songs"
 IF keyval(scF2) > 1 THEN Custom_volume_menu
 IF keyval(scF3) > 1 THEN music_backend_menu
 IF keyval(scF4) > 1 THEN export_songlist

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
  IF keyval(ccLeft) > 1 AND songnum > 0 THEN
   importsong_save_song_data songname, songnum
   songnum -= 1
   state.need_update = YES
  END IF
  IF keyval(ccRight) > 1 AND songnum < 32767 THEN
   importsong_save_song_data songname, songnum
   songnum += 1
   IF needaddset(songnum, gen(genMaxSong), "song") THEN
    songname = ""
    importsong_save_song_data songname, songnum
   END IF
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
 'Allow newlines in metadata but don't wrap, to cut off long comments
 edgeprint metadata, 0, 9 * (UBOUND(menu) + 2), uilook(uiMenuItem), dpage, YES, YES
 edgeprint "F2: change preview volume", pRight, pBottom, uilook(uiMenuItem), dpage

 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
importsong_save_song_data songname, songnum
music_stop
RETURN songnum
END FUNCTION

SUB importsong_import_song_file (songname as string, songfile as string, bamfile as string, byval songnum as integer)
 STATIC default as string
 music_stop
 'closemusic  'music_stop not always enough to cause the music backend to let go of the damn file!
 'setupmusic

 'browse for new song
 DIM sourcesong as string = browse(browseMusic, default, "", "browse_import_song")
 IF sourcesong = "" THEN EXIT SUB

 'Get song name
 DIM newname as string = decode_filename(trimextension(trimpath(sourcesong)))

 'Convert WAV/MP3
 DIM oggtemp as string
 IF getmusictype(sourcesong) = FORMAT_MP3 THEN
  import_convert_mp3 sourcesong, oggtemp
 ELSEIF getmusictype(sourcesong) = FORMAT_WAV THEN
  import_convert_wav sourcesong, oggtemp
 ELSE
  oggtemp = ""
 END IF

 'If conversion to ogg failed, exit
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
 writeablecopyfile sourcesong, songfile

 IF oggtemp <> "" THEN killfile oggtemp

 importsong_save_song_data songname, songnum
END SUB

SUB importsong_get_song_info (songname as string, songfile as string, bamfile as string, byval songnum as integer, file_ext as string, menu() as string, metadata as string, selectable() as bool, state as MenuState)
 music_stop

 'TODO: this is redundant to find_music_lump and getmusictype

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
  songtype = "MPEG Layer III (MP3)"
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

 menu(1) = "<- Song " & songnum & " of " & gen(genMaxSong) & " ->"

 IF songfile = "" THEN
  menu(2) = "-Unused-"
  state.last = 3
  metadata = ""

 ELSE
  menu(2) = "Name: " & songname
  IF bamfile <> songfile AND bamfile <> "" THEN
   menu(6) = "Delete BAM fallback"
   state.last = 6
  ELSE
   state.last = 5
  END IF

  '-- add author, length, etc, info here
  DIM extended_metadata as string
  IF file_ext = ".mp3" THEN
   extended_metadata = read_mp3_metadata(songfile, songtype)
  END IF
  IF file_ext = ".ogg" THEN
   extended_metadata = read_ogg_metadata(songfile)
  END IF

  metadata  = "Type:     " & songtype & !"\n"
  metadata &= "Filesize: " & filesize(songfile) & !"\n"
  IF bamfile <> songfile AND bamfile <> "" THEN
   metadata &= "BAM fallback exists. Filesize: " & filesize(bamfile) & !"\n"
  END IF
  metadata &= extended_metadata

  IF (getmusictype(songfile) AND music_supported_formats()) = 0 THEN
   'Note: music_sdl might report it doesn't support MP3 although it will play it
   metadata &= !"The music backend can't (reliably) play this file type.\n"
  END IF
 END IF

 REDIM PRESERVE selectable(UBOUND(menu))
 FOR i as integer = 0 TO UBOUND(selectable)
  selectable(i) = YES
 NEXT
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
 writeablecopyfile songfile, outfile & file_ext
END SUB

SUB delete_song (byval songnum as integer, songfile as string, bamfile as string)
 IF LEN(songfile) = 0 THEN EXIT SUB
 #IFDEF __FB_WIN32__
  'Only needed on windows, and not currently implemented on unix anyway
  IF channel_to_Game THEN
   'Close Music message
   DIM msg as string = "CM " & songnum
   IF channel_write_line(channel_to_Game, msg) THEN
    channel_wait_for_msg(channel_to_Game, "CM ", "", 1500)
   END IF
  END IF
 #ENDIF
 safekill songfile
 IF LEN(bamfile) THEN safekill bamfile
 'FIXME: handle deleting from rpgdirs (bug 247)... and the same for soundeffects

 IF channel_to_Game THEN send_lump_modified_msg(songfile)  'only need to send any valid filename for this song
END SUB

SUB importsong_save_song_data(songname as string, byval songnum as integer)
 DIM songbuf(dimbinsize(binSONGDATA)) as integer
 writebinstring songname, songbuf(), 0, 30
 storerecord songbuf(), workingdir & SLASH & "songdata.bin", curbinsize(binSONGDATA) \ 2, songnum
END SUB

SUB export_songlist()
 DIM filename as string
 filename = inputfilename("File to export song list to?", ".txt", "", "")
 IF LEN(filename) = 0 THEN EXIT SUB
 DIM fh as integer
 IF OPENFILE(filename + ".txt", FOR_OUTPUT, fh) THEN
  showerror "Couldn't open file"
  EXIT SUB
 END IF
 FOR num as integer = 0 TO gen(genMaxSong)
  PRINT #fh, getsongname(num, YES)
 NEXT
 CLOSE fh
END SUB


'==========================================================================================
'                                Sound Effects Import/Editor
'==========================================================================================

SUB sfx_editor_main ()
 IF read_config_bool("thingbrowser.enable_top_level", YES) THEN
  DIM b as SfxBrowser
  b.browse(-1, , @importsfx)
 ELSE
  importsfx 0
 END IF
END SUB

FUNCTION sfx_picker (recindex as integer = -1) as integer
 DIM b as SfxBrowser
 RETURN b.browse(recindex, , @importsfx, NO)
END FUNCTION

FUNCTION sfx_picker_or_none (recindex as integer = -1) as integer
 DIM b as SfxBrowser
 RETURN b.browse(recindex - 1, YES , @importsfx, NO) + 1
END FUNCTION

FUNCTION importsfx (byval sfxnum as integer) as integer
'sfxnum is the sfx to start with, or > max to add a new one.
'Return value is the last selected sfx, or -1 if cancelling an add-new

IF sfxnum > gen(genMaxSFX) THEN
 'Requested a new one
 IF NOT needaddset(sfxnum, gen(genMaxSFX), "sfx") THEN
  'Cancelled
  RETURN -1
 END IF
END IF

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
 IF keyval(ccCancel) > 1 THEN EXIT DO
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
  IF keyval(ccLeft) > 1 AND sfxnum > 0 THEN
   importsfx_save_sfx_data sfxname, sfxnum
   freesfx sfxnum
   sfxnum -= 1
   importsfx_get_sfx_info sfxname, sfxfile, sfxnum, file_ext, menu(), metadata, selectable(), state
  END IF
  IF keyval(ccRight) > 1 AND sfxnum < maxMaxSFX THEN
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
 'Allow newlines in metadata but don't wrap, to cut off long comments
 edgeprint metadata, 0, 9 * (UBOUND(menu) + 2), uilook(uiMenuItem), dpage, YES, YES
 edgeprint "F2: change preview volume", pRight, pBottom, uilook(uiMenuItem), dpage

 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
importsfx_save_sfx_data sfxname, sfxnum
freesfx sfxnum
RETURN sfxnum
END FUNCTION

SUB importsfx_importsfxfile(sfxname as string, sfxfile as string, byval sfxnum as integer, file_ext as string)
 STATIC default as string

 DIM sourcesfx as string = browse(browseSfx, default, "", "browse_import_sfx")
 IF sourcesfx = "" THEN EXIT SUB

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

 'If conversion to ogg failed, exit
 IF sourcesfx = "" THEN EXIT SUB

 'Delete the old file. We cannot assume it will be overwritten because the extension might change
 IF sfxfile <> "" THEN safekill sfxfile

 sfxname = newname

 '-- calculate lump name
 sfxfile = workingdir & SLASH & "sfx" & sfxnum & "." & LCASE(justextension(sourcesfx))

 '--copy in the new lump
 writeablecopyfile sourcesfx, sfxfile

 IF oggtemp <> "" THEN killfile oggtemp

 '--save and update
 importsfx_save_sfx_data sfxname, sfxnum
END SUB

SUB importsfx_exportsfx(sfxfile as string, file_ext as string, sfxname as string)
 DIM query as string = "Name of file to export to?"
 DIM outfile as string = inputfilename(query, file_ext, "", "input_file_export_sfx", sfxname)
 IF outfile = "" THEN EXIT SUB
 writeablecopyfile sfxfile, outfile & file_ext
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
  sfxtype = "Waveform (WAV)"
 ELSEIF isfile(temp & ".mp3") THEN ' Obsolete, only present in some Ubersetzung WIP games
  file_ext = ".mp3"
  sfxfile = temp & file_ext
  sfxtype = "MPEG Layer III (MP3)"
 END IF

 '--add more formats here

 if sfxfile <> "" then
  'playsfx sfxnum, 0
  sfxname = getsfxname(sfxnum)
 ELSE '--sfx doesn't exist
  sfxname = ""
 END IF

 menu(1) = "<- SFX " & sfxnum & " of " & gen(genMaxSFX) & " ->"
 IF sfxfile = "" THEN
  menu(2) = "-Unused-"
  state.last = 3  ' Hide Export/Delete/Play Sound

  metadata = ""
 ELSE
  menu(2) = "Name: " & sfxname
  state.last = 6  ' Show Export/Delete/Play Sound

  '-- add author, length, etc, info here
  DIM extended_metadata as string
  IF file_ext = ".mp3" THEN
   extended_metadata = read_mp3_metadata(sfxfile, sfxtype)
  END IF
  IF file_ext = ".ogg" THEN
   extended_metadata = read_ogg_metadata(sfxfile)
  END IF

  metadata  = "Type:     " & sfxtype & !"\n"
  metadata &= "Filesize: " & filesize(sfxfile) & !"\n"
  IF filelen(sfxfile) > 1 * 1024 * 1024 THEN
   metadata &= !"Large sound effects can cause pauses when played!\n"
  END IF

  metadata &= extended_metadata

  IF (getmusictype(sfxfile) AND sound_supported_formats()) = 0 THEN
   metadata &= !"This engine build can't play this file\n"
  END IF
 END IF

 REDIM selectable(UBOUND(menu))
 FOR i as integer = 0 TO 6
  selectable(i) = YES
 NEXT
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
  ' mins() stores the minimum allowed values of the gen() elements,
  ' which is offset by 1 from the song/sfx number
  DIM mins(1 TO menusize) as integer
  mins(2) = -1  'Default battle music can be -1, "same as map"

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

    IF keyval(ccCancel) > 1 THEN EXIT DO
    IF keyval(scF1) > 1 THEN show_help "general_music_sfx"
    IF keyval(scF2) > 1 THEN Custom_volume_menu
    usemenu state

    IF enter_space_click(state) THEN
      SELECT CASE state.pt
      CASE 0
        EXIT DO
      CASE 1 TO lastmusicitem
        gen(index(state.pt)) = song_picker_or_none(gen(index(state.pt)))
        state.need_update = YES
        IF gen(index(state.pt)) > 0 THEN
         playsongnum gen(index(state.pt)) - 1
        END IF
      CASE lastmusicitem + 1 TO state.last
        gen(index(state.pt)) = sfx_picker_or_none(gen(index(state.pt)))
        state.need_update = YES
        IF gen(index(state.pt)) > 0 THEN
         playsfx gen(index(state.pt)) - 1
        END IF
      END SELECT
    END IF

    SELECT CASE state.pt
    CASE 1 TO lastmusicitem
      'Music
      IF zintgrabber(gen(index(state.pt)), mins(state.pt) - 1, gen(genMaxSong)) THEN
        music_stop
        state.need_update = YES
      END IF
    CASE lastmusicitem + 1 TO state.last
      'Sound effect
      IF zintgrabber(gen(index(state.pt)), mins(state.pt) - 1, gen(genMaxSFX)) THEN
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
        ELSEIF value = -1 THEN
          IF index(idx) = genBatMus THEN
            disp(idx) = menu(idx) & "Silence"
          ELSE
            disp(idx) = menu(idx) & "None"
          END IF
        ELSEIF value = -2 THEN
          disp(idx) = menu(idx) & "Same as map"
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
