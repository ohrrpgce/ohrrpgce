'OHRRPGCE GAME - Compatibility functions, FreeBasic version
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
option explicit

#include compat.bi
#include allmodex.bi
#include fontdata.bi
#include gfx.bi

'Can do this in FB but not in QB - need another solution
common shared workingdir$, version$, game$

dim shared seg as integer ptr
'bit of a waste, just stores the rpg name from the command line
dim shared storecmd as string

DECLARE SUB fatalerror (e$)
DECLARE FUNCTION small% (n1%, n2%)

SUB dummyclear(arg1%,arg2%,arg3%) 'dummy sub for compatibility
END SUB

SUB getdefaultfont(font() as integer)
	dim i as integer
	
	for i = 0 to 1023
		font(i) = font_data(i)
	next
END SUB

SUB xbloadmap (f$, array%(), e$)
'Just a wrapper in FreeBasic, needed to cater for a bug in QB
	xbload f$, array(), e$
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

sub storecommandline(temppath as string)
'a thinly veiled excuse to get some commandline stuff into FB
'temppath is ignored in the FB version
	dim i as integer = 1
	dim temp as string
	
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
			end if
		else
			'only keep one non-flag argument, hopefully the file
			storecmd = command(i)
		end if
		i = i + 1
	wend
end sub

function getcommandline(temppath as string) as string
	getcommandline = storecmd
end function

FUNCTION canplay (file$)
	'dummy, you should be able to play anything passed in (unless this sub finds uses elsewhere)
	canplay = 1
END FUNCTION

SUB playsongnum (songnum%)
	DIM as string songbase, songfile, numtext
	
	numtext = LTRIM$(STR$(songnum))
	songbase = workingdir$ + SLASH + "song" + numtext
	songfile = ""
	if isfile(songbase + ".mid") then
		'is there a midi?
		songfile = songbase + ".mid"
	else
		'no, get bam name
		IF isfile(songbase + ".bam") THEN
			songfile = songbase + ".bam"
		ELSE
			IF isfile(game$ + "." + numtext) THEN 
				songfile = game$ + "." + numtext
			end if
		END IF
	end if
	IF songfile <> "" THEN loadsong songfile
END SUB

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