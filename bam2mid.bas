'OHRRPGCE - bam2mid
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

''
'' bam2mid -- convert a .BAM file to a General Midi .MID file
''		2005-10-11 - 1.0 - Ignores final infinite loop point and
''		                   sets velocity to 64. Don't know if that
''		                   is good or bad.
''
'' usage: bam2mid <infile> [<outfile>]
'' <outfile> defaults to <infile>.mid if not specified.
''
'' To compile:
''        scons bam2mid
''

#include "util.bi"
#include "common_base.bi"
#include "banks.bi"
#include "crt/stddef.bi"
#include "lumpfile.bi"

#define LOG_BAM(msg)
'#define LOG_BAM(msg) ? msg

#define VELOCITY 		96

#define NOTE_OFF	&b10000000
#define NOTE_ON		&b10010000
#define PATCH_CHANGE 	&b11000000

declare sub setbigval(byval value as integer)
declare sub setsmallval(byval value as integer)
declare function setvarval(byval value as integer) as integer
declare function getvoice(bamvoice as voice) as integer
declare sub magicSysexStart(byval file as integer, byval length as integer)
declare sub magicSysexEnd(byval file as integer)

dim shared bignum(0 to 3) as ubyte => { 0, 0, 0, 0 }
dim shared smallnum(0 to 1) as ubyte => { 0, 0 }

sub bam2mid(infile as string, outfile as string)

	'fields for writing to midi file
	dim magic as string * 4

	dim tracklen as integer = 0
	dim lenpos as integer

	dim bamvoice as voice

	'need to remember the exact note pressed in mids, unlike bams
	dim tracknote(0 to 15) as integer
	'save file positions for loops
	dim labelpos(0 to 15) as integer
	dim loopcount(0 to 15) as integer
	dim returnpos as integer = -1

	dim as integer f1, f2, i, j

	'initialise shared vals
	setbigval(0)
	setsmallval(0)

	for i = 0 to 15
		labelpos(i) = -1
		loopcount(i) = -1
	next
	'Label 0 defaults to the beginning of the file, but it's only used for
	'looping from the end, and we stop instead of processing that loop.
	labelpos(0) = 5  'Past header

	if outfile = "" then
		outfile = infile + ".mid"
	end if

	'open both files
	if openfile(infile, for_binary + access_read, f1) then
		'debug "File " + infile + " could not be opened."
		exit sub
	end if

	get #f1, , magic
	if magic <> "CBMF" then
		debug "File " + infile + " is not a BAM."
		close #f1
		exit sub
	end if

	kill outfile

	if openfile(outfile, for_binary + access_write, f2) then
		'debug "Output file " + outfile + " could not be opened."
		close #f1
		exit sub
	end if

	'write the midi header
	magic = "MThd"
	put #f2, , magic			'chunk type
	bignum(3) = 6
	put #f2, , bignum()			'chunk length
	put #f2, , smallnum()		'file type (type 0 = single track)
	smallnum(1) = 1
	put #f2, , smallnum()		'number of tracks (always 1 for type 0)
	smallnum(1) = 37
	put #f2, , smallnum()		'ticks per 1/4 note (half second, by default)

	'write the track header
	magic = "MTrk"
	put #f2, , magic		'chunk type
	lenpos = seek(f2)		'remember where we are
	bignum(3) = 0
	put #f2, , bignum()		'chunk length - come back and fill this in later

	'process a bam
	dim ub as ubyte
	dim cmd as ubyte
	dim chan as ubyte
	dim delta as integer = 0
	dim bc as integer
	dim mb as ubyte

	'put the tempo in
	bc = setvarval(0)
	fput f2, , @bignum(0), bc	'variable length delta time
	tracklen = tracklen + bc
	mb = &hff
	put #f2, , mb				'metacommand
	mb = &h51
	put #f2, , mb				'end track
	mb = 3
	put #f2, , mb				'no further params
	tracklen = tracklen + 3
	bignum(0) = &h07
	bignum(1) = &hA1
	bignum(2) = &h20
	fput f2, , @bignum(0), 3
	tracklen = tracklen + 3


	get #f1, , ub
	do while not eof(f1)
		if ub < 128 then
			cmd = ub and &hf0
			chan = ub and &h0f
			LOG_BAM(seek(f1) & " cmd " &  cmd & " " & chan)
			select case cmd
				case 0: 'stop song
					'Command 0 is documented as stop song, and the Euphoria and
					'QB players in the BAM Dev Kit treat it that way, but
					'the asm implementation does not - it's unimplemented.
					'The final (2002-11-21) version of fm.asm added a bug where
					'it's misinterpreted as a "start note" command, but earlier
					'(<= 2000-12-12) versions properly ignored it.
					'testfiles/GURGU.BAM is an example of a BAM in the wild
					'(from Timestream Saga) that contains a cmd 0 at the beginning.
					'exit do
				case 16: 'start note
					get #f1, , ub 'get freq
					'write midi note on
					bc = setvarval(delta)
					fput f2, , @bignum(0), bc	'variable length delta time
					tracklen = tracklen + bc
					mb = NOTE_ON or chan
					put #f2, , mb				'command
					put #f2, , ub				'note
					tracknote(chan) = ub 		'remember note
					mb = VELOCITY
					put #f2, , mb				'velocity
					tracklen = tracklen + 3
					delta = 0
				case 32: 'stop note
					'write midi note on
					bc = setvarval(delta)
					fput f2, , @bignum(0), bc	'variable length delta time
					tracklen = tracklen + bc
					mb = NOTE_OFF or chan
					put #f2, , mb				'command
					mb = tracknote(chan)
					put #f2, , mb				'note
					mb = 64
					put #f2, , mb				'velocity
					tracklen = tracklen + 3
					delta = 0
				case 48: 'define instrument
					'write midi patch change
					bc = setvarval(delta)
					fput f2, , @bignum(0), bc	'variable length delta time
					tracklen = tracklen + bc
					get #f1, , bamvoice
					mb = PATCH_CHANGE or chan
					put #f2, , mb
					mb = getvoice(bamvoice)
					put #f2, , mb
					tracklen = tracklen + 2
					delta = 0
				case 80: 'set label
					'save file position
					if chan = 0 then
						'Label 0 is used for looping from the end of the BAM.
						'Its position defaults to the start, but many BAMs
						'explicitly place it at the beginning (after instrument
						'definitions), but it could be elsewhere.
						'BUG: actually, we might perform a jump to a label other
						'than 0. E.g. ARIEDUET.BAM jumps to label 2 at the end.
						'This is pretty annoying to support, though.

						'Set Controller 111: set loop point (RPG Maker-compatible
						'supported by music_native, music_native2 and SDL Mixer X).
						bc = setvarval(delta)
						fput f2, , @bignum(0), bc	'variable length delta time
						tracklen += bc
						mb = &HB0
						put #f2,,mb
						mb = 111
						put #f2,,mb
						mb = 127
						put #f2,,mb
						tracklen += 3
						delta = 0
					end if
					labelpos(chan) = seek(f1)
				case 96: 'jump
					get #f1, , ub 'loop control
					LOG_BAM("  loop " & ub & iif(ub=255, " chorus", iif(ub=254, " jump", " times")))
					if labelpos(chan) > 0 then
						if ub = 255 then
							'chorus loop, but only if not already
							'in a chorus
							if returnpos = -1 then
								returnpos = seek(f1)
								seek f1, labelpos(chan)
							end if
						end if
						if ub = 254 then
							'Infinite loop.
							'An infinite loop to marker 0 always appears at the
							'end of the file. It effectively marks the end of
							'the song. It could appear earlier (e.g. in ARIEDUET.BAM)
							exit do
						end if
						if ub < 254 then
							if loopcount(chan) = -1 then
								loopcount(chan) = ub
							end if
							if loopcount(chan) = 0 then
								loopcount(chan) = -1 'reset
							else
								loopcount(chan) = loopcount(chan) - 1
								seek f1, labelpos(chan)
							end if
						end if
					else
						LOG_BAM("bad loop!")
					end if
				case 112: 'end of chorus
					if returnpos > -1 then
						seek f1, returnpos
						returnpos = -1
					end if
				case else: 'reserved: ignore
					'nothing
			end select
		else
			'wait
			delta = delta + ((ub - 127) * 4)
			LOG_BAM(seek(f1) & " wait " &  (ub - 127))
		end if

		get #f1, , ub
	loop

	'write end track event
	bc = setvarval(delta)
	fput f2, , @bignum(0), bc	'variable length delta time
	tracklen = tracklen + bc
	mb = &hff
	put #f2, , mb				'metacommand
	mb = &h2f
	put #f2, , mb				'end track
	mb = 0
	put #f2, , mb				'no further params
	tracklen = tracklen + 3

	'skip back and write the track length
	setbigval(tracklen)
	put #f2, lenpos, bignum()

	close #f2
	close #f1

end sub

'Convert int32 to big-endian
sub setbigval(byval value as integer)
	bignum(3) = value and &hff
	bignum(2) = (value shr 8) and &hff
	bignum(1) = (value shr 16) and &hff
	bignum(0) = (value shr 24) and &hff
end sub

'Convert int16 to big-endian
sub setsmallval(byval value as integer)
	smallnum(1) = value and &hff
	smallnum(0) = (value shr 8) and &hff
end sub

function setvarval(byval value as integer) as integer
	dim bytes as integer = 0
	dim tmp as integer
	dim b as integer
	dim flag as ubyte = 0

	if value = 0 then
		bignum(0) = 0
		return 1
	end if

	tmp = value
	while tmp > 0
		bytes = bytes + 1
		tmp = tmp \ 128
	wend

	tmp = value
	for b = bytes - 1 to 0 step -1
		bignum(b) = (tmp mod 128) or flag
		flag = &h80
		tmp = tmp \ 128
	next

	return bytes
end function

function getvoice(bamvoice as voice) as integer
'returns GM program number for this instrument
	dim as integer i, j

	'check gm voices
	for i = 0 to 127
		for j = 0 to 10
			if gm_voice(i,j) <> bamvoice.vbyte(j) then
				continue for, for
			end if
		next
		return i
	next

	'check ibank voices
	for i = 0 to 127
		for j = 0 to 10
			if ibank_voice(i,j) <> bamvoice.vbyte(j) then
				continue for, for
			end if
		next
		return ibank_map(i)
	next

	return 0	'default = acoustic grand piano
end function


#ifdef __FB_MAIN__

dim infile as string, outfile as string

if command(1) <> "" then infile = command(1)
if command(2) <> "" then outfile = command(2)

if infile = "" then
	PRINT "Usage: bam2mid filename"
	PRINT "Converts a BAM file into MIDI format"
	system
end if

if outfile = "" then
	outfile = infile
	if right(lcase(outfile), 4) = ".bam" then
		outfile = left(outfile, len(outfile)-4)
	end if
	outfile = outfile & ".mid"
end if


bam2mid(infile, outfile)

#endif
