'OHRRPGCE GAME - Compatibility functions, FreeBasic version
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
option explicit

#include compat.bi
#include allmodex.bi
#include gglobals.bi
#include fontdata.bi
#include gfx.bi

DECLARE SUB fatalerror (e$)
DECLARE FUNCTION small% (n1%, n2%)

'bit of a waste, just stores the rpg name from the command line
dim shared storecmd as string

SUB dummyclear(arg1%,arg2%,arg3%) 'dummy sub for compatibility
END SUB

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

SUB crashexplain()
	PRINT "Please report this exact error message to ohrrpgce@HamsterRepublic.com"
	PRINT "Be sure to describe in detail what you were doing when it happened"
	PRINT
	PRINT version$
	PRINT "Memory Info:"; FRE(0)
#ifndef __FB_LINUX__
	PRINT "Executable: "; progdir$ + exename$ + ".EXE"
#else
	PRINT "Executable: "; progdir$ + exename$
#endif	
	PRINT "RPG file: "; sourcerpg$
END SUB

sub togglewindowed()
	gfx_togglewindowed
end sub

sub storecommandline
'a thinly veiled excuse to get some commandline stuff into FB
	dim i as integer = 1
	dim temp as string
	
	while command(i) <> ""
		temp = left$(command(i), 1)
		'/ should not be a flag under linux
		if temp = "-" or temp = "/" then
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

function getcommandline() as string
	getcommandline = storecmd
end function