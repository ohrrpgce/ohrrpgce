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

extern workingdir$, game$

declare SUB debug (s$)

'Gosub workaround
option dynamic
dim shared gosubbuf(31) as crt_jmp_buf
dim shared gosubptr as integer = 0

'stores the rpg name from the command line without any command-line arguments
dim shared storecmd(7) as string
dim shared cmdargs as integer

'private Subs and functions oly used internally
DECLARE SUB display_help_string(help as string)

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

sub togglewindowed()
	gfx_togglewindowed
end sub

sub processcommandline()
'a thinly veiled excuse to get some commandline stuff into FB
	dim i as integer = 1
	dim temp as string
	dim vtemp as string
	dim arg as integer

	dim show_help as integer = 0
	dim help as string = ""

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
			elseif temp = "v" or temp = "version" then
				show_help = -1
				help = help & long_version$ & LINE_END
				help = help & "(C) Copyright 1997-2007 James Paige and Hamster Republic Productions" & LINE_END
				help = help & "This game engine is free software under the terms of the GPL" & LINE_END
				help = help & "For source-code see http://HamsterRepublic.com/ohrrpgce/source.php" & LINE_END
				help = help & "Game data copyright and license will vary." & LINE_END
			elseif temp = "?" or temp = "help" then
				show_help = -1
				help = help & "-? -help            Display this help screen" & LINE_END
				help = help & "-v -version         Show version and build info" & LINE_END
				help = help & "-f -fullscreen      Start in full-screen mode if possible" & LINE_END
				help = help & "-w -windowed        Start in windowed mode (default)" & LINE_END
				help = help & gfx_describe_options() & LINE_END
			elseif temp = "s" or temp = "smooth" then
				gfx_setoption("smooth", 1)
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
	if show_help then
		display_help_string help
	end if
end sub

sub display_help_string(help as string)
	dim k as string
	print help    ' display to text console (only works on some linux)
	screen 11     ' create a graphical fake text console
	print help    ' display the help on the graphical console
	k = input(1)  ' use FreeBasic-style keypress checking because our keyhandler isn't set up yet
	SYSTEM        ' terminate the program
end sub

function commandlineargcount() as integer
	commandlineargcount = cmdargs
end function

function commandlinearg(argnum as integer) as string
	if argnum <= cmdargs then
		commandlinearg = storecmd(argnum - 1)
	end if
end function

SUB romfontchar (font(), char)
'should I implement this using the default font? potentially useful
'i suppose
END SUB

SUB makedir (dirname$)
MKDIR dirname$
#ifdef __FB_LINUX__
 ' work around broken file permissions in dirs created by linux version
 ' MKDIR creates with mode 644, should create with mode 755
 SHELL "chmod +x """ + dirname$ + """"
#endif
END SUB

SUB setwindowtitle (title as string)
	gfx_windowtitle title
END SUB

FUNCTION ReadShort(fh as integer,p as long=-1) as short
	DIM ret as short
	IF p = -1 THEN
		GET #fh,,ret
	ELSEIF p >= 0 THEN
		GET #fh,p,ret
	END IF
	return ret
END FUNCTION

Sub WriteShort(fh as integer,p as long, v as integer)
	WriteShort(fh,p,cshort(v))
END SUB

Sub WriteShort(fh as integer,p as long, v as short)
	IF p = -1 THEN
		PUT #fh,,v
	ELSEIF p >= 0 THEN
		PUT #fh,p,v
	END IF
END SUB

Function ReadVStr(fh as integer, le as integer) as string
	dim l as short, ret as string, c as short, i as integer
	l = readshort(fh)
	
	for i = 0 to le - 1
		c = readshort(fh)
		if i < l then ret = ret & chr(c AND 255)
	next
	
	return ret
end function

Sub WriteVStr(fh as integer, le as integer, s as string)
	dim i as integer
	writeshort(fh, -1, small(le, len(s)))
	
	for i = 0 to le - 1
		if i < len(s) then writeshort(fh, -1, cint(s[i])) else writeshort(fh, -1, 0)
	next
end sub

function xstr$(x as integer)
	if x >= 0 then
		xstr$ = " " + str$(x)
	else
		xstr$ = str$(x)
	end if
end function

function xstr$(x as short)
	if x >= 0 then
		xstr$ = " " + str$(x)
	else
		xstr$ = str$(x)
	end if
end function

function xstr$(x as single)
	if x >= 0 then
		xstr$ = " " + str$(x)
	else
		xstr$ = str$(x)
	end if
end function

function xstr$(x as double)
	if x >= 0 then
		xstr$ = " " + str$(x)
	else
		xstr$ = str$(x)
	end if
end function

function intstr$(x as integer)
    intstr$ = str$(x)
end function
