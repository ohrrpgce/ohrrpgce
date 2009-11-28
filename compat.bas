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
#include "common.bi"
#include "music.bi"

dim nulzstr as zstring ptr  '(see compat.bi)

'Gosub workaround
option dynamic
dim shared gosubbuf(31) as crt_jmp_buf
dim shared gosubptr as integer = 0
#ifdef timer_variables
dim timer_variables
#endif

'private Subs and functions oly used internally
DECLARE SUB display_help_string(help as string)

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

function commandline_flag(opt as string) as integer
'returns true if opt is a flag (prefixed with -,--,/) and removes the prefix
	dim temp as string
	temp = left(opt, 1)
	'/ should not be a flag under linux
#ifdef __FB_LINUX__
	if temp = "-" then
#else
	if temp = "-" or temp = "/" then
#endif
		temp = mid(opt, 2, 1)
		if temp = "-" then  '--
			opt = mid(opt, 3)
		else
			opt = mid(opt, 2)
		end if
		return YES
	end if
	return NO
end function

function usage_setoption(opt as string, arg as string) as integer
	dim help as string = ""
	if opt = "v" or opt = "version" then
		help = help & long_version$ & LINE_END
		help = help & "(C) Copyright 1997-2007 James Paige and Hamster Republic Productions" & LINE_END
		help = help & "This game engine is free software under the terms of the GNU GPL v2+" & LINE_END
		help = help & "For source-code see http://HamsterRepublic.com/ohrrpgce/source.php" & LINE_END
		help = help & "Game data copyright and license will vary." & LINE_END
		display_help_string help
		return 1
	elseif opt = "?" or opt = "help" then
		help = help & "-? -help            Display this help screen" & LINE_END
		help = help & "-v -version         Show version and build info" & LINE_END
		help = help & "-f -fullscreen      Start in full-screen mode if possible" & LINE_END
		help = help & "-w -windowed        Start in windowed mode (default)" & LINE_END
		help = help & *gfx_describe_options() & LINE_END
		help = help & "-log foldername     Log debug messages to a specific folder" & LINE_END
		display_help_string help
		return 1
	elseif opt = "log" then
		dim d as string = with_orig_path(arg, YES)
		if isdir(d) ANDALSO diriswriteable(d) then
			log_dir = d
			return 2
		else
			help = "log dir """ & d & """ is not valid." & LINE_END
			help = help & "a valid argument to -log must be a writable folder that exists."
			display_help_string help
			return 1
		end if
	end if
	return 0
end function

function with_orig_path(file_or_dir as string, add_slash as integer=0) as string
	dim d as string = file_or_dir
	if not is_absolute_path(d) then d = orig_dir & SLASH & d
	if add_slash and right(d, 1) <> SLASH then d = d & SLASH
	return d
end function

sub processcommandline()
'populates cmdline_args, passes arguments around
	dim i as integer = 1
	dim argsused as integer
	dim opt as string
	dim arg as string

	while command(i) <> ""
		argsused = 0

		opt = command(i)
		if commandline_flag(opt) then
			arg = command(i + 1)
			if commandline_flag(arg) then arg = ""

			argsused = gfx_setoption(opt, arg)
			if argsused = 0 then argsused = allmodex_setoption(opt, arg)
			if argsused = 0 then argsused = usage_setoption(opt, arg)
		end if
		'debug "opt = " & opt & " arg = " & arg & " used = " & argsused

		if argsused = 0 then
			'everything else falls through and is stored for Game/Custom to catch
			'(we could prehaps move their handling into functions as well)
			'note index 0 not used (FB arrays... so inconvenient)
			str_array_append(cmdline_args(), command(i))
			argsused = 1
			'debug ubound(cmdline_args) & ": stored " & command(i)
		end if
		i += argsused
	wend
end sub

sub display_help_string(help as string)
	dim k as string
	print help    ' display to text console (only works on some linux)
	screen 11     ' create a graphical fake text console
	print help    ' display the help on the graphical console
	k = input(1)  ' use FreeBasic-style keypress checking because our keyhandler isn't set up yet
	SYSTEM        ' terminate the program
end sub

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

FUNCTION ReadShort(fh as integer,p as long=-1) as short
	DIM ret as short
	IF p = -1 THEN
		GET #fh,,ret
	ELSEIF p >= 0 THEN
		GET #fh,p,ret
	END IF
	return ret
END FUNCTION

FUNCTION ReadShort(filename as string, p as integer) as short
	DIM ret as short
	DIM fh as integer
	fh = FREEFILE
	OPEN filename for binary access read as #fh
	GET #fh, p, ret
	CLOSE #fh
	return ret
END FUNCTION

FUNCTION ReadByte(fh as integer,p as long=-1) as ubyte
	DIM ret as ubyte
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

Sub WriteShort(filename as string, p as integer, v as integer)
	DIM fh as integer
	fh = FREEFILE
	OPEN filename FOR BINARY AS #fh
	PUT #fh, p, cshort(v)
	CLOSE #fh
END SUB

Sub WriteByte(fh as integer,v as ubyte, p as long=-1)
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

Function ReadByteStr(fh as integer, le as integer) as string
	dim l as short, ret as string, c as ubyte, i as integer
	l = readshort(fh)
	
	for i = 0 to le - 1
		c = readbyte(fh)
		if i < l then ret = ret & chr(c)
	next
	
	return ret
end function

Sub WriteByteStr(fh as integer, le as integer, s as string)
	dim i as integer
	writeshort(fh, -1, small(le, len(s)))
	
	for i = 0 to le - 1
		if i < len(s) then writebyte(fh, cubyte(s[i])) else writebyte(fh, 0)
	next
end sub

function xstr (x as integer) as string
	if x >= 0 then
		xstr = " " + str(x)
	else
		xstr = str(x)
	end if
end function
