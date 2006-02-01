'' 
'' music_sdl.bas - External music functions implemented in SDL.
''
'' part of OHRRPGCE - see elsewhere for license details
''

option explicit

#include music.bi
#include "allegro.bi"

'this should be in allegro.bi but isn't
#define MIDI_AUTODETECT -1

'extern
declare sub debug(s$)
declare sub bam2mid(infile as string, outfile as string)
declare function isfile(n$) as integer

dim shared music_on as integer = 0
dim shared music_vol as integer
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
	'assumes allegro is already active for gfx, will need an 
	'allegro_init call otherwise
	if music_on = 0 then
		install_sound(DIGI_AUTODETECT, MIDI_AUTODETECT, 0)
		
		music_vol = 8
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

sub music_play(songname as string, fmt as music_format)
	if music_on = 1 then
		songname = rtrim$(songname)	'lose any added nulls
		
		if fmt = FORMAT_BAM then
			dim midname as string
			midname = songname + ".mid"
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
			songname = songname + ".mid"
			fmt = FORMAT_MIDI		end if

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

sub music_setvolume(vol as integer)
	music_vol = vol
	if music_on = 1 then
		'multiply by 17 to adjust for change in scale
		'(17 * 15 = 255)
		set_volume(-1, music_vol * 17)
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

