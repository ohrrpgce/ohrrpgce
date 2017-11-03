'OHRRPGCE - common routines for inspecting (but not playing) or finding audio files
'(C) Copyright 2017 James Paige/OHRRPGCE developers
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "config.bi"
#include "util.bi"
#include "common.bi"
#include "string.bi"
#include "audiofile.bi"
'#ifdef HAVE_VORBISFILE
#include "vorbis/vorbisfile.bi"
'#endif

dim shared libvorbisfile as any ptr

extern "C"
#undef ov_clear
#undef ov_fopen
#undef ov_info
#undef ov_bitrate
#undef ov_time_total
#undef ov_comment
dim shared ov_clear as function(byval vf as OggVorbis_File ptr) as long
dim shared ov_fopen as function(byval path as const zstring ptr, byval vf as OggVorbis_File ptr) as long
dim shared ov_info as function(byval vf as OggVorbis_File ptr, byval link as long) as vorbis_info ptr
dim shared ov_bitrate as function(byval vf as OggVorbis_File ptr, byval link as long) as clong
dim shared ov_time_total as function(byval vf as OggVorbis_File ptr, byval link as long) as double
dim shared ov_comment as function(byval vf as OggVorbis_File ptr, byval link as long) as vorbis_comment ptr
end extern


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
	dim fh as integer
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

#macro MUSTLOAD(hfile, procedure)
	procedure = dylibsymbol(hfile, #procedure)
	if procedure = NULL then
		dylibfree(hFile)
		hFile = NULL
		return NO
	end if
#endmacro

private function _load_libvorbisfile(libfile as string) as bool
	libvorbisfile = dylibload(libfile)
	if libvorbisfile = NULL then return NO

	MUSTLOAD(libvorbisfile, ov_clear)
	MUSTLOAD(libvorbisfile, ov_fopen)
	MUSTLOAD(libvorbisfile, ov_info)
	MUSTLOAD(libvorbisfile, ov_bitrate)
	MUSTLOAD(libvorbisfile, ov_time_total)
	MUSTLOAD(libvorbisfile, ov_comment)
	return YES
end function

' Dynamically load functions from libvorbisfile.
' This isn't really necessary! However it avoids errors if:
' -you use an old copy of SDL_mixer.dll that's laying around
' -on Mac you're trying to run ohrrpgce-custom directly without bundling
'  (compiling instructions on the wiki tell you to install a standard SDL_mixer.framework in /Library/Frameworks)
' -libvorbisfile isn's installed on Unix
private function load_vorbisfile() as bool
	if libvorbisfile then return YES
	' Unix
	if _load_libvorbisfile("vorbisfile") then return YES
	' libvorbisfile is statically linked into our windows and mac SDL_mixer builds.
	' We can load them even if we're using a different music backend
	if _load_libvorbisfile("SDL_mixer") then return YES
	RETURN NO
end function

' First index is number of channels, second is quality
dim oggenc_quality_levels(1 to 2, -1 to 10) as integer = { _
	{32, 48, 60, 70, 80, 86, 96, 110, 120, 140, 160, 240}, _
	{45, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 500} _
}

'Return the oggenc quality level most closely matching this bitrate. Assumes 44.1kHz.
function oggenc_quality(channels as integer, bitrate as integer) as integer
        if bitrate <= 0 then return -1
        channels = bound(channels, 1, 2)
        dim bestmatch as double = 999
        dim bestidx as integer
        for idx as integer = lbound(oggenc_quality_levels, 2) to ubound(oggenc_quality_levels, 2)
                dim ratio as double = bitrate / oggenc_quality_levels(channels, idx)
                if ratio < 1 then ratio = 1 / ratio
                if ratio < bestmatch then bestmatch = ratio : bestidx = idx
        next
        return bestidx
end function

' Return one or more lines of text describing bitrate, sample rate, channels, and comments of an .ogg Vorbis file
function read_ogg_metadata(songfile as string) as string
	if load_vorbisfile() = NO then
		return "Can't read OGG metadata: missing library"
	end if
	'We don't unload libvorbisfile afterwards. No need.

'#ifdef HAVE_VORBISFILE
	dim oggfile as OggVorbis_File
	dim errcode as integer
	errcode = ov_fopen(songfile, @oggfile)
	if errcode then
		dim msg as string
		select case errcode
			case OV_EREAD:      msg = "ERROR: Can't read file!"
			case OV_ENOTVORBIS: msg = "ERROR: Not a Vorbis (audio) .ogg file!"
			case OV_EVERSION:   msg = "ERROR: Unknown .ogg format version!"
			case OV_EBADHEADER: msg = "ERROR: Corrupt .ogg file!"
			case else:          msg = "ERROR: Can't parse file (error " & errcode & ")"
		end select
		debug "ov_fopen( " & songfile & ") failed : " & msg
		return msg
	end if

	dim ret as string

	' Length
	dim length as double
	length = ov_time_total(@oggfile, -1)
	if length <> OV_EINVAL then
		dim msg as string = "Length:   "
		if length >= 60 then
			msg &= (int(length) \ 60) & "m"
			'Avoid printing 1m60.0s for length 119.99
			length = small(fmod(length, 60), 59.9)
			msg &= format(length, "00.0") & "s"
		else
			msg &= format(length, "0.0") & "s"
		end if
		ret &= msg & !"\n"
	else
		debug "ov_time_total failed on " & songfile
	end if

	' Bit and sample rate, channels
	dim info as vorbis_info ptr
	info = ov_info(@oggfile, -1)
	if info then
		ret &= info->channels & " channel(s)  " &  format(info->rate / 1000, "0.0") & !"kHz  \n"
	else
		debug "ov_info failed on " & songfile
	end if

	' Bitrate
	dim msg as string
	dim bitrate as integer = ov_bitrate(@oggfile, -1)
	if bitrate > 0 then
		msg &= cint(bitrate / 1000) & "kbps"
	elseif info andalso info->bitrate_nominal > 0 then
		' Don't show both the average and the nominal bitrates, that's too much info
		msg &= cint(info->bitrate_nominal / 1000) & "kbps (Nominal)"
	end if
	if info andalso info->bitrate_nominal > 0 andalso in_bound(info->channels, 1, 2) then
		msg &= " (quality ~" & oggenc_quality(info->channels, info->bitrate_nominal \ 1000) & ")"
	end if
	if msg <> "" then ret &= "Bitrate:  " & msg & !"\n"

	' Comments
	dim comments as vorbis_comment ptr
	comments = ov_comment(@oggfile, -1)
	if comments then
		ret &= !"\n"
		for idx as integer = 0 TO comments->comments - 1
			' .ogg comment strings are UTF8, not null-terminated
			' They're usually formatted like "AUTHOR=virt", but sometimes lower case or no 'tag' name.
			dim ucmmt as ustring, cmmt as string
			ucmmt = blob_to_string(comments->user_comments[idx], comments->comment_lengths[idx])
			cmmt = utf8_to_OHR(ucmmt)
			dim as integer eqpos = instr(cmmt, "="), spcpos = instr(cmmt, " ")
			if eqpos > 1 andalso (spcpos = 0 orelse spcpos > eqpos) then
				'Seems to be a tag, format it like "Author: virt"
				cmmt = titlecase(left(cmmt, eqpos - 1)) & ": " & mid(cmmt, eqpos + 1)
			end if
                        ret &= cmmt & !"\n"
		next
	else
		debug "ov_comment failed: " & songfile
	end if

	ov_clear(@oggfile)
	return rtrim(ret)
' #else
' 	return "(OGG metadata not enabled in this build)"
' #endif
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
