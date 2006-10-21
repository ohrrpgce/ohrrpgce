'OHRRPGCE GAME - Compatibility functions, FreeBasic version
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
option explicit

#include "compat.bi"
#include "allmodex.bi"
#include "fontdata.bi"
#include "gfx.bi"
#include "util.bi"
#include "music.bi"

extern workingdir$, version$, game$

declare SUB debug (s$)

'Gosub workaround
option dynamic
dim shared gosubbuf(31) as crt_jmp_buf
dim shared gosubptr as integer = 0

dim shared seg as integer ptr
'stores the rpg name from the command line without any command-line arguments
dim shared storecmd(7) as string
dim shared cmdargs as integer

DECLARE SUB fatalerror (e$)

SUB getdefaultfont(font() as integer)
	dim i as integer

	for i = 0 to 1023
		font(i) = font_data(i)
	next
END SUB

SUB xbload (f$, array(), e$)

	IF isfile(f$) THEN
		DIM ff%, byt as UByte, seg AS Short, offset AS Short, length AS Short
		dim ilength as integer
		dim i as integer

		ff = FreeFile
		OPEN f$ FOR BINARY AS #ff
		GET #ff,, byt 'Magic number, always 253
		IF byt <> 253 THEN fatalerror e$
		GET #ff,, seg 'Segment, no use anymore
		GET #ff,, offset 'Offset into the array, not used now
		GET #ff,, length 'Length
		'length is in bytes, so divide by 2, and subtract 1 because 0-based
		ilength = (length / 2) - 1

		dim buf(ilength) as short

		GET #ff,, buf()
		CLOSE #ff

		for i = 0 to small(ilength, ubound(array))
			array(i) = buf(i)
		next i

		ELSE
		fatalerror e$
	END IF

END SUB

SUB xbsave (f$, array%(), bsize%)

	DIM ff%, byt as UByte, seg AS uShort, offset AS Short, length AS Short
	dim ilength as integer
	dim i as integer
	dim needbyte as integer

	seg = &h9999
	offset = 0
	'Because we're working with a short array, but the data is in bytes
	'we need to check if there's an odd size, and therefore a spare byte
	'we'll need to add at the end.
	ilength = (bsize \ 2) - 1	'will lose an odd byte in the division
	needbyte = bsize mod 2		'write an extra byte at the end?
	length = bsize	'bsize is in bytes
	byt = 253

	'copy array to shorts
	DIM buf(ilength) as short
	for i = 0 to small(ilength, ubound(array))
		buf(i) = array(i)
	next

	ff = FreeFile
	OPEN f$ FOR BINARY ACCESS write AS #ff
	PUT #ff, , byt				'Magic number
	PUT #ff, , seg				'segment - obsolete
	PUT #ff, , offset			'offset - obsolete
	PUT #ff, , length			'size in bytes

	PUT #ff,, buf()
	if needbyte = 1 then
		i = small(ilength + 1, ubound(array)) 'don't overflow
		byt = array(i) and &hff
		put #ff, , byt
	end if
	CLOSE #ff

END SUB

SUB crashexplain()
	PRINT "Please report this exact error message to ohrrpgce@HamsterRepublic.com"
	PRINT "Be sure to describe in detail what you were doing when it happened"
	PRINT
	PRINT version$
	PRINT "Memory Info:"; FRE(0)
	PRINT "Executable: "; exepath + command(0)
'	PRINT "RPG file: "; sourcerpg$
END SUB

'replacements for def seg and peek, use seg shared ptr
'assumes def seg will always be used to point to an integer and
'that integers are only holding 2 bytes of data
sub defseg(var as integer ptr)
	seg = var
end sub

function xpeek(byval idx as integer) as integer
	dim as ubyte bval
	dim as integer hilow

	hilow = idx mod 2
	idx = idx \ 2

	if hilow = 0 then
		bval = seg[idx] and &hff
	else
		bval = (seg[idx] and &hff00) shr 8
	end if
	xpeek = bval
end function

sub xpoke(byval idx as integer, byval v as integer)
	dim as integer bval
	dim as integer hilow
	dim as integer newval

	hilow = idx mod 2
	idx = idx \ 2

	bval = v and &hff
	if hilow = 0 then
		newval = seg[idx] and &hff00
		seg[idx] = newval or bval
	else
		newval = seg[idx] and &hff
		seg[idx] = newval or (bval shl 8)
	end if
end sub

sub togglewindowed()
	gfx_togglewindowed
end sub

sub processcommandline()
'a thinly veiled excuse to get some commandline stuff into FB
	dim i as integer = 1
	dim temp as string
	dim vtemp as string
	dim arg as integer

	cmdargs = 0

	while command(i) <> ""
		temp = left$(command(i), 1)
		'/ should not be a flag under linux
#ifdef __FB_LINUX__
		if temp = "-" then
#else
		if temp = "-" or temp = "/" then
#endif
			'option
			temp = mid$(command(i), 2)
			if temp = "w" or temp = "windowed" then
				gfx_setwindowed(1)
			elseif temp = "f" or temp = "fullscreen" then
				gfx_setwindowed(0)
			else
				'get next argument and check it is numeric
				i = i + 1
				vtemp = command(i)
				if len(vtemp) > 0 then
					if asc(vtemp, 1) >= &h30 and asc(vtemp, 1) <= &h39 then
						'first char is numeric, assume we're okay
						arg = valint(vtemp)
						if temp = "z" or temp = "zoom" then
							gfx_setoption("zoom", arg) ' 1 or 2
						elseif temp = "b" or temp = "border" then
							gfx_setoption("border", arg) ' 0 or 1
						elseif temp = "d" or temp = "depth" then
							gfx_setoption("depth", arg) ' 8, 24 or 32
						end if
					else
						'ignore this parameter and set i back
						i = i - 1
					end if
				end if
			end if
		else
			'store non-flag arguments
			storecmd(cmdargs) = command(i)
			cmdargs = small(cmdargs + 1, 8)
		end if
		i = i + 1
	wend
end sub

function commandlineargcount() as integer
	commandlineargcount = cmdargs
end function

function commandlinearg(argnum as integer) as string
	if argnum <= cmdargs then
		commandlinearg = storecmd(argnum - 1)
	end if
end function

SUB playsongnum (songnum%)
	DIM as string songbase, songfile, numtext

	numtext = STR$(songnum)
	songbase = workingdir$ + SLASH + "song" + numtext
	songfile = DIR$(songbase + ".*")
	if songfile = "" then exit sub

	loadsong workingdir$ + SLASH + songfile
END SUB

FUNCTION getmusictype (file$)

	DIM ext$, chk
	ext$ = lcase(justextension(file$))

	'special case
	if str(cint(ext$)) = ext$ then return FORMAT_BAM

	SELECT CASE ext$
	CASE "bam"
		chk = FORMAT_BAM
	CASE "mid"
		chk = FORMAT_MIDI
	CASE "xm"
		chk = FORMAT_XM
	CASE "it"
	  chk = FORMAT_IT
	CASE "wav"
	  chk = FORMAT_WAV
	CASE "ogg"
	  chk = FORMAT_OGG
	CASE "mp3"
	  chk = FORMAT_MP3
	CASE "s3m"
	  chk = FORMAT_S3M
	CASE ELSE
	  debug "unknown format: " & file$ & " - " & ext$
	END SELECT

  return chk
END FUNCTION

FUNCTION validmusicfile (file$, types = FORMAT_BAM AND FORMAT_MIDI)
'-- actually, doesn't need to be a music file, but only multi-filetype imported data right now
	DIM ext$, a$, realhd$, musfh, v, chk
	ext$ = lcase(justextension(file$))
	chk = getmusictype(file$)

	if chk AND types = 0 then return 0


	SELECT CASE chk
	CASE FORMAT_BAM
		a$ = "    "
		realhd$ = "CBMF"
		v = 1
	CASE FORMAT_MIDI
		a$ = "    "
		realhd$ = "MThd"
		v = 1
	CASE FORMAT_XM
		a$ =      "                 "
		realhd$ = "Extended Module: "
		v = 1
	END SELECT

	if v then
  	musfh = FREEFILE
  	OPEN file$ FOR BINARY AS #musfh
	  GET #musfh, 1, a$
	  CLOSE #musfh
	  IF a$ <> realhd$ THEN return 0
	end if

	return 1
END FUNCTION

SUB romfontchar (font(), char)
'should I implement this using the default font? potentially useful
'i suppose

'regs.ax = &H1130
'regs.bx = &H300
'CALL interruptx(&H10, regs, regs)
'off9 = regs.bx: seg9 = regs.es
'DEF SEG = regs.es
''FOR i = 1 TO 255
'FOR j = 0 TO 7
' b = PEEK(regs.bp + (8 * char) + j)
' FOR k = 0 TO 7
'  setbit font(), char * 4, (7 - k) * 8 + j, (b AND 2 ^ k)
' NEXT k
'NEXT j
''NEXT i

END SUB

SUB makedir (dirname$)
MKDIR dirname$
#ifdef __FB_LINUX__
 ' work around broken file permissions in dirs created by linux version
 ' MKDIR creates with mode 644, should create with mode 755
 SHELL "chmod +x " + dirname$
#endif
END SUB

SUB setwindowtitle (title as string)
	gfx_windowtitle title
END SUB

FUNCTION ReadShort(fh as integer,p as long) as short
	DIM ret as short
	IF p = -1 THEN
		GET #fh,,ret
	ELSEIF p >= 0 THEN
		GET #fh,p,ret
	END IF
	return ret
END FUNCTION

Sub WriteShort(fh as integer,p as long, v as integer)
	IF p = -1 THEN
		PUT #fh,,cshort(v)
	ELSEIF p >= 0 THEN
		PUT #fh,p,cshort(v)
	END IF
END SUB

