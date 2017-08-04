'OHRRPGCE - common routines for inspecting (but not playing) or finding audio files
'(C) Copyright 2017 James Paige/OHRRPGCE developers
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "config.bi"
#include "util.bi"
#include "common.bi"
#include "audiofile.bi"

'==========================================================================================
'                                    Examining Files
'==========================================================================================

'Not used anywhere!
function isawav(fi as string) as bool
	if not isfile(fi) then return NO 'duhhhhhh

#define ID(a,b,c,d) asc(a) SHL 0 + asc(b) SHL 8 + asc(c) SHL 16 + asc(d) SHL 24
	dim _RIFF as integer = ID("R","I","F","F") 'these are the "signatures" of a
	dim _WAVE as integer = ID("W","A","V","E") 'wave file. RIFF is the format,
	'dim _fmt_ as integer = ID("f","m","t"," ") 'WAVE is the type, and fmt_ and
	'dim _data as integer = ID("d","a","t","a") 'data are the chunks
#undef ID

	dim chnk_ID as integer
	dim chnk_size as integer
	dim fh as integer = freefile
	openfile(fi, for_binary + access_read, fh)

	get #fh, , chnk_ID
	if chnk_ID <> _RIFF then
		close #fh
		return NO 'not even a RIFF file
	end if

	get #fh, , chnk_size 'don't care

	get #fh, , chnk_ID

	if chnk_ID <> _WAVE then
		close #fh
		return NO 'not a WAVE file, pffft
	end if

	'is this good enough? meh, sure.
	close #fh
	return YES
end function

' Check that an audio file really is the format it appears to be
' (This isn't and was never really necessary...)
FUNCTION valid_audio_file (filepath as string) as bool
 DIM as string hdmask, realhd
 DIM as integer musfh, chk
 chk = getmusictype(filepath)

 SELECT CASE chk
  CASE FORMAT_BAM
   hdmask = "    "
   realhd = "CBMF"
  CASE FORMAT_MIDI
   hdmask = "    "
   realhd = "MThd"
  CASE FORMAT_XM
   hdmask = "                 "
   realhd = "Extended Module: "
  'Other supported module formats are missing, but I don't see any point adding them
 END SELECT

 IF LEN(hdmask) THEN
  musfh = FREEFILE
  OPENFILE(filepath, FOR_BINARY, musfh)
  GET #musfh, 1, hdmask
  CLOSE #musfh
  IF hdmask <> realhd THEN return NO
 END IF

 RETURN YES
END FUNCTION

function getmusictype (file as string) as MusicFormatEnum
	if real_isfile(file) = NO then
		'no further checking if this is a directory
		return 0
	end if

	dim ext as string, chk as integer
	ext = lcase(justextension(file))

	'special case
	if str(cint(ext)) = ext then return FORMAT_BAM

	select case ext
	case "bam"
		chk = FORMAT_BAM
	case "mid"
		chk = FORMAT_MIDI
	case "xm"
		chk = FORMAT_XM
	case "it"
		chk = FORMAT_IT
	case "wav"
		chk = FORMAT_WAV
	case "ogg"
		chk = FORMAT_OGG
	case "mp3"
		chk = FORMAT_MP3
	case "s3m"
		chk = FORMAT_S3M
	case "mod"
		chk = FORMAT_MOD
	case else
		debug "unknown format: " & file & " - " & ext
		chk = 0
	end select

	return chk
end function


'==========================================================================================
'                                     Music/SFX Lumps
'==========================================================================================


function find_music_lump(songnum as integer) as string
  DIM songbase as string, songfile as string

  songbase = workingdir & SLASH & "song" & songnum
  songfile = ""

  IF isfile(songbase & ".mp3") THEN
    songfile = songbase & ".mp3"
  ELSEIF isfile(songbase & ".ogg") THEN
    songfile = songbase & ".ogg"
  ELSEIF isfile(songbase & ".mod") THEN
    songfile = songbase & ".mod"
  ELSEIF isfile(songbase & ".xm") THEN
    songfile = songbase & ".xm"
  ELSEIF isfile(songbase & ".s3m") THEN
    songfile = songbase & ".s3m"
  ELSEIF isfile(songbase & ".it") THEN
    songfile = songbase & ".it"
  ELSEIF isfile(songbase & ".mid") THEN
    songfile = songbase & ".mid"
  ELSEIF isfile(songbase & ".bam") THEN
    songfile = songbase & ".bam"
  ELSEIF isfile(game & "." & songnum) THEN
    songfile = game & "." & songnum ' old-style BAM naming scheme
  END IF
  RETURN songfile
END FUNCTION

' Translate sfx number to lump name
function find_sfx_lump (sfxnum as integer) as string
	dim as string sfxbase

	sfxbase = workingdir & SLASH & "sfx" & sfxnum
	if isfile(sfxbase & ".ogg") THEN
		return sfxbase & ".ogg"
	elseif isfile(sfxbase & ".mp3") then
		return sfxbase & ".mp3"
	elseif isfile(sfxbase & ".wav") then
		return sfxbase & ".wav"
	else
		return ""
	end if
end function
