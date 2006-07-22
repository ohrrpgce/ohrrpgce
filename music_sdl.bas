'' 
'' music_sdl.bas - External music functions implemented in SDL.
''
'' part of OHRRPGCE - see elsewhere for license details
''

option explicit

#include music.bi
#include "SDL\SDL.bi"
#include "SDL\SDL_mixer.bi"

'extern
declare sub debug(s$)
declare sub bam2mid(infile as string, outfile as string, useOHRm as integer)
declare function isfile(n$) as integer

dim shared music_on as integer = 0
'dim shared music_song as FMOD_SOUND ptr = 0 'integer = 0
'dim shared fmod as FMOD_SYSTEM ptr
'dim shared fmod_channel as FMOD_CHANNEL ptr = 0
dim shared music_vol as integer
dim shared music_paused as integer
dim shared music_song as Mix_Music ptr = NULL
dim shared orig_vol as integer = -1

'The music module needs to manage a list of temporary files to
'delete when closed, mainly for custom, so they don't get lumped
type delitem
	fname as zstring ptr
	nextitem as delitem ptr
end type

dim shared delhead as delitem ptr = null

sub music_init()	
	dim version as uinteger
	if music_on = 0 then
		dim audio_rate as integer
		dim audio_format as Uint16
		dim audio_channels as integer
		dim audio_buffers as integer
	
		' We're going to be requesting certain things from our audio
		' device, so we set them up beforehand
		audio_rate = MIX_DEFAULT_FREQUENCY
		audio_format = MIX_DEFAULT_FORMAT
		audio_channels = 2
		audio_buffers = 4096
		
'		SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO)
		SDL_Init(SDL_INIT_AUDIO)
		
		if (Mix_OpenAudio(audio_rate, audio_format, audio_channels, audio_buffers)) <> 0 then
			Debug "Can't open audio"
			music_on = -1
			SDL_Quit()
			exit sub
		end if
		
		music_vol = 8
		music_on = 1
		music_paused = 0
	end if	
end sub

sub music_close()
	if music_on = 1 then
		if orig_vol > 0 then
			'restore original volume
			Mix_VolumeMusic(orig_vol)
		else
			'arbitrary medium value
			Mix_VolumeMusic(64)
		end if
		
		if music_song <> 0 then
			Mix_FreeMusic(music_song)
			music_song = 0
			music_paused = 0
		end if
		
		Mix_CloseAudio
		SDL_Quit
		music_on = 0
		
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
	end if
end sub

sub music_play(songname as string, fmt as music_format)
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
			midname = songname + "-" + lcase(hex(flen)) + ".bmd"
			'check if already converted
			if isfile(midname) = 0 then
				bam2mid(songname, midname,0)
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
			Mix_FreeMusic(music_song)
			music_song = 0
			music_paused = 0
		end if

		music_song = Mix_LoadMUS(songname)
		if music_song = 0 then
			debug "Could not load song " + songname
			exit sub
		end if
		
		Mix_PlayMusic(music_song, -1)			
		music_paused = 0

		'not really working when songs are being faded in.
		if orig_vol = -1 then
			orig_vol = Mix_VolumeMusic(-1)
		end if
					
		'dim realvol as single
		'realvol = music_vol / 15
		'FMOD_Channel_SetVolume(fmod_channel, realvol)
		if music_vol = 0 then
			Mix_VolumeMusic(0)
		else
			'add a small adjustment because 15 doesn't go into 128
			Mix_VolumeMusic((music_vol * 8) + 8)
		end if
	end if
end sub

sub music_pause()
	'Pause is broken in SDL_Mixer, so just stop.
	music_stop
' 	if music_on = 1 then
' 		if music_song > 0 then
' 			if music_paused = 0 then
' 				Mix_PauseMusic	'doesn't seem to work
' 				music_paused = 1
' 			end if
' 		end if
' 	end if
end sub

sub music_resume()
	if music_on = 1 then
		if music_song > 0 then
			Mix_ResumeMusic
			music_paused = 0
		end if
	end if
end sub

sub music_stop()
	if music_on = 1 then
		if music_song > 0 then
			Mix_HaltMusic
		end if
	end if
end sub

sub music_setvolume(vol as integer)
	music_vol = vol
	if music_on = 1 then
		if music_vol = 0 then
			Mix_VolumeMusic(0)
		else
			'add a small adjustment because 15 doesn't go into 128
			Mix_VolumeMusic((music_vol * 8) + 8)
		end if
	end if
end sub

function music_getvolume() as integer
	music_getvolume = music_vol
end function

sub music_fade(targetvol as integer)
'Unlike the original version, this will pause everything else while it
'fades, so make sure it doesn't take too long
	dim vstep as integer = 1
	dim i as integer
	
	if music_vol > targetvol then vstep = -1
	for i = music_vol to targetvol step vstep
		music_setvolume(i)
		sleep 10
	next	
end sub


DECLARE sub SDL_done_playing cdecl(byval channel as integer)

TYPE sound_effect
  used as integer 'whether this slot is free
  
  paused as integer
  playing as integer
  
  pause_pos as integer
  
  buf as Mix_Chunk ptr
  
  chan as integer
  
END TYPE

dim shared sfx_slots(7) as sound_effect


dim shared sound_inited as integer 'must be non-zero for anything but _init to work

sub sound_init
  'if this were called twice, the world would end.
  if sound_inited then exit sub
  
  'anything that might be initialized here is done in music_init
  'but, I must do it here too
   music_init
  Mix_channelFinished(@SDL_done_playing)
  sound_inited = 1
end sub

sub sound_close
  'trying to free something that's already freed... bad!
  if not sound_inited then exit sub
  
  dim i as integer

    for i = 0 to 7
    with sfx_slots(i)
      if .used then
        Mix_FreeChunk(.buf)
        .paused = 0
        .playing = 0
        .used = 0
        .buf = 0
      end if
    end with
  next
  
  
  
  sound_inited = 0
end sub

function sound_load(byval slot as integer, f as string) as integer
  'slot is the sfx_slots element to use, or -1 to automatically pick one
  'f is the file.
  dim i as integer

  if slot = -1 then
    for i = 0 to ubound(sfx_slots)
    
      if not sfx_slots(i).used then
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
    .buf = Mix_LoadWAV(@f[0])

    if .buf = NULL then return -1
  end with

  return slot 'yup, that's all
  
end function

sub sound_free(byval slot as integer)
  with sfx_slots(slot)
    if .used then
      .used = 0
      .playing = 0
      .paused = 0
      mix_freechunk(.buf)
    end if
  end with
end sub


sub sound_play(byval slot as integer, byval l as integer)
  with sfx_slots(slot)
    if not .used then exit sub
    if (.playing<>0) and (.paused=0) then exit sub
    if not .buf then exit sub
    
    if .paused then
      Mix_Resume(.chan)
      .paused = 0
    else
      if l then l = -1
      .chan = mix_playchannel(-1,.buf,l)
      .playing = 1
    end if

  end with
end sub

sub sound_pause(byval slot as integer)
  with sfx_slots(slot)
    if not .used then exit sub
    if not .playing then exit sub
    if .paused then exit sub
    
    .paused = 1
    Mix_Pause(.chan)
  end with
end sub

sub sound_stop(byval slot as integer)
  with sfx_slots(slot)
    if not .used then exit sub
    if not .playing then exit sub
    
    .playing = 0
    .paused = 0
    
    Mix_HaltChannel(.chan)
  end with
end sub

function sound_playing(byval slot as integer) as integer
  with sfx_slots(slot)
    if not .used then return 0
    
    return .playing
    
  end with
end function

function sound_slots as integer
  return ubound(sfx_slots)
end function

sub SDL_done_playing cdecl(byval channel as integer)
  dim i as integer

  for i = 0 to ubound(sfx_slots)
    if sfx_slots(i).chan = channel then sfx_slots(i).playing = 0
  next
end sub

