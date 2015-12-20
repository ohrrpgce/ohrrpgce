'' 
'' music_allegro.bas - External music functions implemented in Allegro
''
'' part of OHRRPGCE - see elsewhere for license details
''
#print
#print WARNING: music_allegro compiles, but the sound effects interface is unimplemented.
#print

#include "music.bi"
#include "util.bi"
#include "common.bi"

#undef font
#include "allegro.bi"


'this should be in allegro.bi but isn't (for old FB anyway)
#ifndef MIDI_AUTODETECT
#define MIDI_AUTODETECT -1
#endif

extern allegro_initialised as bool

'''' Module-shared variables

dim shared music_on as integer = 0
dim shared music_vol as single
dim shared music_paused as integer
dim shared music_song as MIDI ptr = 0
'dim shared orig_vol as integer = -1

'The music module needs to manage a list of temporary files to
'delete when closed, mainly for custom, so they don't get lumped
type delitem
	fname as zstring ptr
	nextitem as delitem ptr
end type

dim shared delhead as delitem ptr = null


sub music_init()
	if allegro_initialised = NO then
		allegro_init()
		allegro_initialised = YES
	end if

	if music_on = 0 then
		install_sound(DIGI_AUTODETECT, MIDI_AUTODETECT, 0)
		
		music_vol = 0.5
		music_on = 1
		music_paused = 0
		
		music_setvolume(music_vol)
	end if	
end sub

sub music_close()
	if music_on = 1 then
		'Let Allegro shut down the music systems on exit
		'just stop the song, if playing
		if music_song <> 0 then
			destroy_midi(music_song)
			music_song = 0
			music_paused = 0
		end if
		
		if delhead <> null then
			'delete temp files
			dim ditem as delitem ptr
			dim dlast as delitem ptr
			
			ditem = delhead
			while ditem <> null
				if isfile(*(ditem->fname)) then
					kill *(ditem->fname)
				end if
				deallocate ditem->fname 'deallocate string
				dlast = ditem
				ditem = ditem->nextitem
				deallocate dlast 'deallocate delitem
			wend
			delhead = null
		end if
		'music_on = 0
	end if
end sub

function music_get_info() as string
	return allegro_id
end function

sub music_play(songname as string, fmt as MusicFormatEnum)
	if music_on = 1 then
		songname = rtrim$(songname)	'lose any added nulls
		
		if fmt = FORMAT_BAM then
			dim midname as string
			dim as integer bf, flen
			'get length of input file
			bf = freefile
			open songname for binary access read as #bf
			flen = lof(bf)
			close #bf
			'use last 3 hex digits of length as a kind of hash, 
			'to verify that the .bmd does belong to this file
			flen = flen and &h0fff
			midname = tmpdir & trimpath(songname) & "-" & lcase(hex(flen)) & ".bmd"
			'check if already converted
			if isfile(midname) = 0 then
				bam2mid(songname, midname)
				'add to list of temp files
				dim ditem as delitem ptr
				if delhead = null then
					delhead = allocate(sizeof(delitem))
					ditem = delhead
				else
					ditem = delhead
					while ditem->nextitem <> null
						ditem = ditem->nextitem
					wend
					ditem->nextitem = allocate(sizeof(delitem))
					ditem = ditem->nextitem
				end if
				ditem->nextitem = null
				'allocate space for zstring
				ditem->fname = allocate(len(midname) + 1)
				*(ditem->fname) = midname 'set zstring
			end if
			songname = midname
			fmt = FORMAT_MIDI
		end if

		'stop current song
		if music_song <> 0 then
			destroy_midi(music_song)
			music_song = 0
			music_paused = 0
		end if

		music_song = load_midi(songname)
		if music_song = 0 then
			debug "Could not load song " + songname
			exit sub
		end if
		
		play_midi(music_song, 1)			
		music_paused = 0

		'if orig_vol = -1 then
		'	orig_vol = Mix_VolumeMusic(-1)
		'end if
	end if
end sub

sub music_pause()
	if music_on = 1 then
		if music_song > 0 then
			if music_paused = 0 then
				midi_pause
				music_paused = 1
			end if
		end if
	end if
end sub

sub music_resume()
	if music_on = 1 then
		if music_song > 0 then
			midi_resume
			music_paused = 0
		end if
	end if
end sub

sub music_stop()
	music_pause()
end sub

sub music_setvolume(byval vol as single)
	music_vol = vol
	if music_on = 1 then
		'This sets only MIDI volume
		set_volume(-1, music_vol * 255)
	end if
end sub

function music_getvolume() as single
	music_getvolume = music_vol
end function

TYPE sound_effect
  used as integer 'whether this slot is free
  
  paused as integer
  playing as integer
  
  pause_pos as integer
  
  buf as SAMPLE ptr
  voice as integer
END TYPE

dim shared sfx_slots(7) as sound_effect


dim shared sound_inited as integer 'must be non-zero for anything but _init to work


sub sound_init
  'if this were called twice, the world would end.
  if sound_inited then exit sub
  
  'anything that might be initialized here is done in music_init
  'but, I must do it here too
  music_init
end sub

sub sound_close
  'trying to free something that's already freed... bad!
  if sound_inited = 0 then exit sub
  
  dim i as integer
  
  for i = 0 to 7
    with sfx_slots(i)
      if .used then
        deallocate_voice(.voice)
        destroy_sample(.buf)
        .paused = 0
        .playing = 0
        .used = 0
        .buf = 0
      end if
    end with
  next
  
  'let allegro clean up for me
  
  sound_inited = 0
end sub

'UNIMPLEMENTED
sub sound_reset() : end sub

function sound_load(byval slot as integer, f as string) as integer
  'slot is the sfx_slots element to use, or -1 to automatically pick one
  'f is the file.
  dim i as integer
  
  if slot = -1 then
    for i = 0 to ubound(sfx_slots)
    
      if sfx_slots(i).used = 0 then
        slot = i
        exit for
      end if
    next
    
    if slot = ubound(sfx_slots) + 1 then return -1 'no free slots...
  end if
  
  with sfx_slots(slot)
    if .used then
      sound_free(slot)
    end if
    
    .used = 1
    .buf = load_wav(f)
    
    if .buf = NULL then return -1
    
    .voice = allocate_voice(.buf)
    
    if .voice = -1 then return -1
  end with
  
  return slot 'yup, that's all
  
end function

sub sound_free(byval slot as integer)
  with sfx_slots(slot)
    if .used then
      .used = 0
      .playing = 0
      .paused = 0
      deallocate_voice(.voice)
      destroy_sample(.buf)
    end if
  end with
end sub

'num UNIMPLEMENTED
sub sound_play(byval num as integer, byval loopcount as integer, byval slot as integer = 0)
  with sfx_slots(slot)
    if .used = 0 then exit sub
    if .playing and .paused = 0 then exit sub
    if .buf = 0 then exit sub
    
    if loopcount then
      voice_set_playmode(.voice,PLAYMODE_LOOP)
    else
      voice_set_playmode(.voice,PLAYMODE_PLAY)
    end if
    
    .paused = 0
    .playing = 1
    voice_start(.voice)
  end with
end sub

'num UNIMPLEMENTED
sub sound_pause(byval num as integer, byval slot as integer = 0)
  with sfx_slots(slot)
    if .used = 0 then exit sub
    if .playing = 0 then exit sub
    if .paused then exit sub
    
    .paused = 1
    voice_stop(.voice)
  end with
end sub

'num UNIMPLEMENTED
sub sound_stop(byval num as integer, byval slot as integer = 0)
  with sfx_slots(slot)
    if .used = 0 then exit sub
    if .playing = 0 then exit sub
    
    .playing = 0
    .paused = 0
    
    voice_stop(.voice)
    voice_set_position(.voice,0)
  end with
end sub

'num UNIMPLEMENTED
function sound_playing(byval num as integer, byval slot as integer = 0) as bool
  with sfx_slots(slot)
    if .used = 0 then return NO
    
    return voice_get_position(.voice) <> -1
    
  end with
end function

function sound_slots as integer
  return ubound(sfx_slots)
end function

'UNIMPLEMENTED
function LoadSound overload(filename as string,  byval num as integer = -1) as integer
	return 0
end function

'UNIMPLEMENTED
sub UnloadSound(byval num as integer) : end sub

