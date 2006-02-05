'' 
'' music_win.bas - External music functions implemented in Windows
''                 API (MCI)
''
'' part of OHRRPGCE - see elsewhere for license details
''

option explicit

#include music.bi
#include "windows.bi"
#include "win\mmsystem.bi"

'extern
declare sub debug(s$)
declare sub bam2mid(infile as string, outfile as string)
declare function isfile(n$) as integer

dim shared music_on as integer = 0
dim shared music_vol as integer
dim shared music_paused as integer
dim shared music_song as MCI_OPEN_PARMS ptr = NULL
dim shared orig_vol as integer = -1

'The music module needs to manage a list of temporary files to
'delete when closed, mainly for custom, so they don't get lumped
type delitem
	fname as zstring ptr
	nextitem as delitem ptr
end type

dim shared delhead as delitem ptr = null

sub music_init()	
	'doesn't seem to be needed
	music_on = 1
end sub

sub music_close()
	if music_on = 1 then
		if orig_vol > -1 then
			'restore original volume
'			Mix_VolumeMusic(orig_vol)
		end if
		
		if music_song <> 0 then
			' Close the device, and wait for this operation to complete
			mciSendCommand(music_song->wDeviceID, MCI_CLOSE, MCI_WAIT, cast(DWORD, music_song))
			deallocate music_song
			music_song = 0
			music_paused = 0
		end if
		
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
	dim errcode as integer
	
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
			fmt = FORMAT_MIDI
		end if

		'stop current song
		if music_song <> 0 then
			' Close the device, and wait for this operation to complete
			mciSendCommand(music_song->wDeviceID, MCI_CLOSE, MCI_WAIT, music_song)
			deallocate music_song
			music_song = 0
			music_paused = 0
		end if

		music_song = callocate(sizeof(MCI_OPEN_PARMS))
		music_song->lpstrDeviceType = MCI_DEVTYPE_SEQUENCER
	
		' Specify the MIDI file we wish operated upon (ie, played) 
		music_song->lpstrElementName = @songname[0]

		'open the device		
		errcode = mciSendCommand(0, MCI_OPEN, MCI_WAIT or MCI_OPEN_ELEMENT or MCI_OPEN_TYPE or MCI_OPEN_TYPE_ID, music_song)
		if (errcode <> 0) then
			debug "Could not load song " + songname + " error=" + str(errcode)
			mciSendCommand(music_song->wDeviceID, MCI_CLOSE, MCI_WAIT, music_song)
			deallocate music_song
			music_song = 0
			exit sub
		end if
		
		'play it
		mciSendCommand(music_song->wDeviceID, MCI_PLAY, 0, music_song)
		music_paused = 0

		if orig_vol = -1 then
' 			orig_vol = Mix_VolumeMusic(-1)
		end if
					
		if music_vol = 0 then
' 			Mix_VolumeMusic(0)
		else
			'add a small adjustment because 15 doesn't go into 128
' 			Mix_VolumeMusic((music_vol * 8) + 8)
		end if
	end if
end sub

sub music_pause()
	if music_on = 1 then
		if music_song > 0 then
			if music_paused = 0 then
				mciSendCommand(music_song->wDeviceID, MCI_PAUSE, 0, music_song)
' 				Mix_PauseMusic	'doesn't seem to work
				music_paused = 1
			end if
		end if
	end if
end sub

sub music_resume()
	if music_on = 1 then
		if music_song > 0 then
			mciSendCommand(music_song->wDeviceID, MCI_RESUME, 0, music_song)
' 			Mix_ResumeMusic
			music_paused = 0
		end if
	end if
end sub

sub music_setvolume(vol as integer)
	music_vol = vol
	if music_on = 1 then
		if music_vol = 0 then
' 			Mix_VolumeMusic(0)
		else
			'add a small adjustment because 15 doesn't go into 128
' 			Mix_VolumeMusic((music_vol * 8) + 8)
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

